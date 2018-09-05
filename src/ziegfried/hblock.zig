const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const params = @import("params.zig");

//===========================================================================//

const HyperblockMask = @IntType(false, params.chunk_denominator);

const HyperblockMaskShift =
    @IntType(false, std.math.log2_int_ceil(u32, params.chunk_denominator));

pub fn HyperblockHeader(comptime Heap: type) type {
    return packed struct {
        /// This field always stores params.hyperblock_magic_number.
        magic_number: u32,
        /// The heap that this hyperblock belongs to.
        heap: *Heap,
        /// A bitmask indicating which chunks in the hyperblock are currently
        /// allocated (1) or free (0).  The lowest bit indicates the first
        /// chunk, and the second-to-highest bit indicates the last chunk (the
        /// highest bit is unused).
        allocated_mask: HyperblockMask,
        /// Intrusive doubly-linked list node.
        list_node: HyperblockListNode,
    };
}

pub fn Hyperblock(comptime Heap: type) type {
    return packed struct {
        const ThisType = this;

        header: HyperblockHeader(Heap),
        _padding: [params.chunk_size - @sizeOf(HyperblockHeader(Heap))]u8,
        chunks: [params.hyperblock_num_chunks][params.chunk_size]u8,

        pub fn init(heap: *Heap, hyperblock_list: *HyperblockList(Heap),
                    free_span_lists: *FreeSpanLists(Heap),
                    child_allocator: *Allocator) !*Hyperblock(Heap) {
            // Allocate a new hyperblock from the child allocator:
            var hyperblocks = try child_allocator.alignedAlloc(
                Hyperblock(Heap), params.chunk_size, 1);
            var hyperblock = &hyperblocks[0];
            // Initialize header values:
            hyperblock.header.magic_number = params.hyperblock_magic_number;
            hyperblock.header.heap = heap;
            hyperblock.header.allocated_mask = 1;
            hyperblock.header.list_node.init();
            // Initialize header for free span:
            var free_span = hyperblock.freeSpanStartingAt(0);
            free_span.init(hyperblock, params.hyperblock_num_chunks);
            free_span_lists.insert(free_span);
            // Add the hyperblock to the list.
            hyperblock_list.insert(hyperblock);
            return hyperblock;
        }

        pub fn deinit(self: *ThisType, child_allocator: *Allocator) void {
            assert(self.isEmpty());
            var free_span = self.freeSpanStartingAt(0);
            assert(free_span.hyperblock == self);
            assert(free_span.num_chunks == params.hyperblock_num_chunks);
            free_span.list_node.remove();
            self.header.list_node.remove();
            child_allocator.destroy(self);
        }

        /// Returns true if none of the chunks in this hyperblock are currently
        /// allocated, false otherwise.
        pub fn isEmpty(self: *const ThisType) bool {
            return self.header.allocated_mask == 0;
        }

        pub fn allocFrom(self: *ThisType, span: *FreeSpanHeader(Heap),
                         size: usize,
                         free_span_lists: *FreeSpanLists(Heap)) []u8 {
            assert(span.hyperblock == self);
            assert(span.num_chunks > 0);
            assert(@ptrToInt(span) % params.chunk_size == 0);
            assert(@ptrToInt(span) >= @ptrToInt(&self.chunks[0]));
            const start_chunk =
                @divExact(@ptrToInt(span) - @ptrToInt(&self.chunks[0]),
                          params.chunk_size);
            assert(start_chunk < params.hyperblock_num_chunks);
            assert(start_chunk + span.num_chunks <=
                   params.hyperblock_num_chunks);
            const num_chunks = numChunksForSize(size);
            assert(num_chunks >= 1);
            assert(num_chunks <= params.allocated_span_max_num_chunks);
            assert(num_chunks <= span.num_chunks);
            const mask = ((HyperblockMask(1) << num_chunks) - 1) <<
                @intCast(HyperblockMaskShift, start_chunk);
            assert(self.header.allocated_mask & ~mask == 0);
            // Update free span header.
            span.list_node.remove();
            if (span.num_chunks > num_chunks) {
                const new_start = start_chunk + num_chunks;
                const new_free_span =
                    @ptrCast(*FreeSpanHeader(Heap),
                             @alignCast(params.chunk_size,
                                        &self.chunks[new_start]));
                new_free_span.init(self, span.num_chunks - num_chunks);
                free_span_lists.insert(new_free_span);
            }
            self.header.allocated_mask |= mask;
            // Put a pointer to this hyperblock at the end of the last chunk.
            assert(num_chunks * usize(params.chunk_size) -
                   @sizeOf(@typeOf(self)) >= size);
            @ptrCast(*?*ThisType,
                     @alignCast(@sizeOf(?*ThisType),
                                &self.chunks[start_chunk + num_chunks - 1]
                                [params.chunk_size - @sizeOf(?*ThisType)])).* =
                self;
            // TODO: In Debug mode, memset the allocated memory to 0xaa.
            // TODO: In Debug mode, memset rest of allocated span to 0xdd.
            return @ptrCast([*]u8, &self.chunks[start_chunk])[0..size];
        }

        pub fn free(self: *ThisType, old_mem: []u8,
                    free_span_lists: *FreeSpanLists(Heap)) void {
            assert(old_mem.len <= params.allocated_span_max_size);
            assert(@ptrToInt(old_mem.ptr) % params.chunk_size == 0);
            assert(@ptrToInt(old_mem.ptr) >= @ptrToInt(&self.chunks[0]));
            const start_chunk =
                @intCast(HyperblockMaskShift,
                         @divExact(@ptrToInt(old_mem.ptr) -
                                       @ptrToInt(&self.chunks[0]),
                                   params.chunk_size));
            assert(start_chunk < params.hyperblock_num_chunks);
            const num_chunks = numChunksForSize(old_mem.len);
            assert(num_chunks >= 1);
            assert(num_chunks <= params.allocated_span_max_num_chunks);
            assert(start_chunk + num_chunks <= params.hyperblock_num_chunks);
            const mask = ((HyperblockMask(1) << num_chunks) - 1) <<
                @intCast(HyperblockMaskShift, start_chunk);
            assert(self.header.allocated_mask & mask == mask);
            // Merge prev/next free spans, if any.
            var prev_free_span: ?*FreeSpanHeader(Heap) = blk: {
                if (start_chunk > 0 and
                        self.header.allocated_mask &
                        @shlExact(HyperblockMask(1),
                                  (start_chunk - 1)) == 0) {
                    var span = self.freeSpanStartingAt(
                        HyperblockMask.bit_count -
                            @clz(self.header.allocated_mask &
                                     (@shlExact(HyperblockMask(1),
                                                start_chunk) - 1)));
                    span.list_node.remove();
                    break :blk span;
                }
                break :blk null;
            };
            var next_free_span: ?*FreeSpanHeader(Heap) = blk: {
                if (start_chunk + num_chunks < params.hyperblock_num_chunks and
                        (self.header.allocated_mask >>
                             (start_chunk + num_chunks)) & 1 == 0) {
                    var span =
                        self.freeSpanStartingAt(start_chunk + num_chunks);
                    span.list_node.remove();
                    break :blk span;
                }
                break :blk null;
            };
            var new_free_span =
                prev_free_span orelse self.freeSpanStartingAt(start_chunk);
            new_free_span.init(
                self,
                (if (prev_free_span) |span| span.num_chunks else 0) +
                    num_chunks +
                    (if (next_free_span) |span| span.num_chunks else 0));
            free_span_lists.insert(new_free_span);
            // Mark this span as free.
            self.header.allocated_mask &= ~mask;
        }

        fn freeSpanStartingAt(self: *ThisType,
                              idx: usize) *FreeSpanHeader(Heap) {
            return @ptrCast(*FreeSpanHeader(Heap),
                            @alignCast(params.chunk_size, &self.chunks[idx]));
        }

        pub fn numChunksForSize(size: usize) HyperblockMaskShift {
            const num_chunks =
                @divFloor(size + @sizeOf(*ThisType) + params.chunk_size - 1,
                          params.chunk_size);
            assert(num_chunks <= params.hyperblock_num_chunks);
            return @intCast(HyperblockMaskShift, num_chunks);
        }

        pub fn allocSpanHyperblockPtr(mem: []u8) ?*ThisType {
            if (mem.len > params.allocated_span_max_size or
                    @ptrToInt(mem.ptr) % params.chunk_size != 0) {
                return null;
            }
            const num_chunks = numChunksForSize(mem.len);
            return @intToPtr(*?*ThisType,
                             @ptrToInt(mem.ptr) +
                                 params.chunk_size * usize(num_chunks) -
                                 @sizeOf(?*ThisType)).*;
        }
    };
}


pub const HyperblockListNode = packed struct {
    next: *HyperblockListNode,
    prev: *HyperblockListNode,

    fn init(self: *this) void {
        self.next = self;
        self.prev = self;
    }

    fn insert_after(self: *this, other: *this) void {
        self.prev = other;
        self.next = other.next;
        other.next.prev = self;
        other.next = self;
    }

    fn remove(self: *this) void {
        self.prev.next = self.next;
        self.next.prev = self.prev;
    }
};

pub fn HyperblockList(comptime Heap: type) type {
    return struct {
        node: HyperblockListNode,

        /// Initializes this list to empty.
        pub fn init(self: *this) void {
            self.node.init();
        }

        /// Frees all hyperblocks in this list.
        fn deinit(self: *this, child_allocator: *Allocator) void {
            while (!self.isEmpty()) {
                const header =
                    @fieldParentPtr(HyperblockHeader(Heap), "list_node",
                                    self.node.next);
                const hyperblock =
                    @fieldParentPtr(Hyperblock(Heap), "header", header);
                hyperblock.deinit(child_allocator);
            }
        }

        /// Returns true if the list is empty.
        fn isEmpty(self: *const this) bool {
            assert((self.node.next == &self.node) ==
                   (self.node.prev == &self.node));
            return self.node.next == &self.node;
        }

        fn insert(self: *this, hyperblock: *Hyperblock(Heap)) void {
            hyperblock.header.list_node.insert_after(&self.node);
        }
    };
}

//===========================================================================//

pub fn FreeSpanHeader(comptime Heap: type) type {
    return struct {
        hyperblock: *Hyperblock(Heap),
        list_node: FreeSpanListNode,
        num_chunks: u32,

        fn init(self: *this, hyperblock: *Hyperblock(Heap),
                num_chunks: u32) void {
            assert(num_chunks >= 1);
            assert(num_chunks <= params.hyperblock_num_chunks);
            assert(@ptrToInt(self) % params.chunk_size == 0);
            assert(@ptrToInt(self) > @ptrToInt(hyperblock));
            assert(@ptrToInt(self) <
                       @ptrToInt(hyperblock) + params.hyperblock_size);
            self.hyperblock = hyperblock;
            self.list_node.init();
            self.num_chunks = num_chunks;
            // TODO: In debug mode, memset the rest of the span to 0xdd
        }
    };
}

pub const FreeSpanListNode = struct {
    next: *FreeSpanListNode,
    prev: *FreeSpanListNode,

    fn init(self: *this) void {
        self.next = self;
        self.prev = self;
    }

    fn insert_after(self: *this, other: *this) void {
        self.prev = other;
        self.next = other.next;
        other.next.prev = self;
        other.next = self;
    }

    fn remove(self: *this) void {
        self.prev.next = self.next;
        self.next.prev = self.prev;
    }
};

pub fn FreeSpanList(comptime Heap: type) type {
    return struct {
        node: FreeSpanListNode,

        /// Initializes this list to empty.
        pub fn init(self: *this) void {
            self.node.init();
        }

        /// Returns true if the list is empty.
        fn isEmpty(self: *const this) bool {
            assert((self.node.next == &self.node) ==
                   (self.node.prev == &self.node));
            return self.node.next == &self.node;
        }

        /// Returns the free span at the head of the list, or null if the list
        /// is empty.
        fn head(self: *const this) ?*FreeSpanHeader(Heap) {
            if (self.isEmpty()) {
                return null;
            } else {
                return @fieldParentPtr(FreeSpanHeader(Heap), "list_node",
                                       self.node.next);
            }
        }

        fn insert(self: *this, free_span: *FreeSpanHeader(Heap)) void {
            free_span.list_node.insert_after(&self.node);
        }
    };
}

pub fn FreeSpanLists(comptime Heap: type) type {
    return struct {
        lists: [params.allocated_span_max_num_chunks]FreeSpanList(Heap),

        pub fn init(self: *this) void {
            for (self.lists) |*list| {
                list.init();
            }
        }

        pub fn deinit(self: *this) void {
            for (self.lists) |*list| {
                assert(list.isEmpty());
            }
        }

        pub fn lastList(self: *this) *FreeSpanList(Heap) {
            return &self.lists[params.allocated_span_max_num_chunks - 1];
        }

        pub fn insert(self: *this, free_span: *FreeSpanHeader(Heap)) void {
            self.listForNumChunks(free_span.num_chunks).insert(free_span);
        }

        fn listForNumChunks(self: *this,
                                num_chunks: usize) *FreeSpanList(Heap) {
            assert(num_chunks >= 1);
            if (num_chunks > params.allocated_span_max_num_chunks) {
                return self.lastList();
            } else {
                return &self.lists[num_chunks - 1];
            }
        }
    };
}

//===========================================================================//

test "HyperblockHeader fits in a chunk" {
    comptime assert(@sizeOf(HyperblockHeader(@OpaqueType())) <=
                    params.chunk_size);
}

test "Hyperblock struct size is correct" {
    comptime assert(@sizeOf(Hyperblock(@OpaqueType())) ==
                    params.hyperblock_size);
}

test "FreeSpanHeader fits in a chunk" {
    comptime assert(@sizeOf(FreeSpanHeader(@OpaqueType())) <=
                    params.chunk_size);
}

test "Hyperblock.numChunksForSize" {
    const HB = Hyperblock(@OpaqueType());
    assert(HB.numChunksForSize(1) == 1);
    // Storing `params.chunk_size` bytes actually takes two chunks, because we
    // also have to store the hyperblock pointer.
    assert(HB.numChunksForSize(params.chunk_size) == 2);
    assert(HB.numChunksForSize(params.chunk_size - @sizeOf(*HB)) == 1);
    assert(HB.numChunksForSize(params.chunk_size * 10 + 100) == 11);
}

test "Hyperblock alloc and free" {
    var direct = std.heap.DirectAllocator.init();
    defer direct.deinit();
    const Heap = i32;
    var heap: Heap = 0;
    var free_span_lists: FreeSpanLists(Heap) = undefined;
    free_span_lists.init();
    defer free_span_lists.deinit();
    var hyperblocks: HyperblockList(Heap) = undefined;
    hyperblocks.init();
    defer hyperblocks.deinit(&direct.allocator);

    var hyperblock =
        try Hyperblock(Heap).init(&heap, &hyperblocks, &free_span_lists,
                                  &direct.allocator);
    var last_list = free_span_lists.lastList();
    assert(last_list.node.next != &last_list.node);
    var free_span = @fieldParentPtr(FreeSpanHeader(Heap), "list_node",
                                    last_list.node.next);
    assert(free_span.hyperblock == hyperblock);
    assert(free_span.num_chunks == params.hyperblock_num_chunks);
    var mem = hyperblock.allocFrom(free_span, 1000, &free_span_lists);
    assert(mem.len == 1000);
    hyperblock.free(mem, &free_span_lists);
}

//===========================================================================//
