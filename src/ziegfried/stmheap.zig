const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const params = @import("params.zig");

const hblock = @import("hblock.zig");
const FreeSpanList = hblock.FreeSpanList(STMHeap);
const FreeSpanLists = hblock.FreeSpanLists(STMHeap);
const Hyperblock = hblock.Hyperblock(STMHeap);
const HyperblockList = hblock.HyperblockList(STMHeap);

//===========================================================================//

const STMHeap = struct {
    free_span_lists: FreeSpanLists,
    hyperblocks: HyperblockList,

    pub fn init(child_allocator: *Allocator) !*STMHeap {
        var self = try child_allocator.createOne(STMHeap);
        self.free_span_lists.init();
        self.hyperblocks.init();
        return self;
    }

    pub fn deinit(self: *this, child_allocator: *Allocator) void {
        self.hyperblocks.deinit(child_allocator);
        self.free_span_lists.deinit();
        child_allocator.destroy(self);
    }

    pub fn alloc(self: *this, size: usize, child_allocator: *Allocator) ![]u8 {
        assert(size <= params.span_max_size);
        const num_chunks = Hyperblock.numChunksForSize(size);
        assert(num_chunks >= 1);
        assert(num_chunks <= params.span_max_num_chunks);
        // Look for an existing free span large enough for this allocation.
        // TODO: make this search loop a method of FreeSpanLists
        for (self.free_span_lists.lists[(num_chunks - 1)..]) |*list| {
            if (list.head()) |free_span| {
                return free_span.hyperblock.allocFrom(
                    free_span, size, &self.free_span_lists);
            }
        }
        // If there are no free spans large enough, then allocate a new
        // hyperblock, and then allocate from that.
        var hyperblock =
            try Hyperblock.init(self, &self.hyperblocks, &self.free_span_lists,
                                child_allocator);
        var free_span =
            self.free_span_lists.lastList().head() orelse unreachable;
        return hyperblock.allocFrom(free_span, size, &self.free_span_lists);
    }

    pub fn free(self: *this, old_mem: []u8, child_allocator: *Allocator) void {
        assert(old_mem.len <= params.span_max_size);
        if (Hyperblock.allocSpanHyperblockPtr(old_mem)) |hyperblock| {
            assert(hyperblock.header.magic_number ==
                       params.hyperblock_magic_number);
            hyperblock.free(old_mem, &self.free_span_lists);
        } else {
            child_allocator.freeFn(child_allocator, old_mem);
        }
    }
};

//===========================================================================//

const STMAllocator = struct {
    pub allocator: Allocator,
    child_allocator: *Allocator,
    heap: *STMHeap,

    pub fn init(child_allocator: *Allocator) !STMAllocator {
        return STMAllocator{
            .allocator = Allocator{
                .allocFn = alloc,
                .reallocFn = realloc,
                .freeFn = free,
            },
            .child_allocator = child_allocator,
            .heap = try STMHeap.init(child_allocator),
        };
    }

    pub fn deinit(self: *this) void {
        self.heap.deinit(self.child_allocator);
    }

    fn alloc(allocator: *Allocator, size: usize,
             alignment: u29) Allocator.Error![]u8 {
        const self = @fieldParentPtr(STMAllocator, "allocator", allocator);
        if (size > params.span_max_size) {
            return try self.child_allocator.allocFn(self.child_allocator, size,
                                                    alignment);
        }
        assert(alignment <= params.chunk_size);
        return try self.heap.alloc(size, self.child_allocator);
    }

    fn realloc(allocator: *Allocator, old_mem: []u8, new_size: usize,
               alignment: u29) Allocator.Error![]u8 {
        @panic("STMAllocator.realloc is not yet implemented"); // TODO
    }

    fn free(allocator: *Allocator, old_mem: []u8) void {
        const self = @fieldParentPtr(STMAllocator, "allocator", allocator);
        if (old_mem.len > params.span_max_size) {
            self.child_allocator.freeFn(self.child_allocator, old_mem);
        } else {
            self.heap.free(old_mem, self.child_allocator);
        }
    }
};

//===========================================================================//

test "STMAllocator alloc and free" {
    var direct = std.heap.DirectAllocator.init();
    var stm = try STMAllocator.init(&direct.allocator);
    defer stm.deinit();
    var slice = try stm.allocator.alloc(u8, 1500);
    defer stm.allocator.free(slice);
    assert(slice.len == 1500);
}

//===========================================================================//
