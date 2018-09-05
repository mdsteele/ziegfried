const builtin = @import("builtin");
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const params = @import("params.zig");

//===========================================================================//

// The Superblock struct is kept packed so that we can rely on the alignments
// of positions within its `blocks` array.  This in turn obligates us to
// declare SuperblockHeader and SuperblockListNode packed as well.

pub fn SuperblockHeader(comptime Heap: type) type {
    return packed struct {
        /// This field always stores params.superblock_magic_number.
        magic_number: u32,
        /// The block size for this superblock.
        block_size: u32,
        /// The number of blocks in this superblock.
        num_blocks: u32,
        /// The number of blocks in this superblock that aren't currently
        /// allocated.
        num_free_blocks: u32,
        /// The head of the freelist.
        first_free_block: ?[*]u8,
        /// Intrusive doubly-linked list node.
        list_node: SuperblockListNode,
        /// The heap that this superblock belongs to.
        heap: *Heap,
    };
}

pub fn Superblock(comptime Heap: type) type {
    return packed struct {
        header: SuperblockHeader(Heap),
        blocks: [params.superblock_size - @sizeOf(SuperblockHeader(Heap))]u8,

        pub fn init(block_size: u32, heap: *Heap,
                    child_allocator: *Allocator) !*Superblock(Heap) {
            assert(block_size >= params.min_block_size);
            assert(block_size <= params.max_block_size);
            // Allocate a new superblock from the child allocator:
            var superblocks = try child_allocator.alignedAlloc(
                Superblock(Heap), params.superblock_size, 1);
            var superblock = &superblocks[0];
            assert(@ptrToInt(superblock) % params.superblock_size == 0);
            // Initialize bookkeeping header values:
            superblock.header.magic_number = params.superblock_magic_number;
            superblock.header.block_size = block_size;
            superblock.header.num_blocks =
                @divFloor(@intCast(u32, superblock.blocks.len), block_size);
            superblock.header.num_free_blocks = superblock.header.num_blocks;
            superblock.header.heap = heap;
            superblock.header.list_node.prev = &superblock.header.list_node;
            superblock.header.list_node.next = &superblock.header.list_node;
            // Initialize block free list:
            var next: ?[*]u8 = null;
            var offset: usize = superblock.blocks.len;
            while (offset >= block_size) {
                offset -= block_size;
                const ptr: [*]u8 = superblock.blocks[offset..].ptr;
                @ptrCast(*?[*]u8, @alignCast(params.min_block_size, ptr)).* =
                    next;
                next = ptr;
            }
            superblock.header.first_free_block = next;
            return superblock;
        }

        pub fn deinit(self: *this, child_allocator: *Allocator) void {
            self.header.list_node.prev.next = self.header.list_node.next;
            self.header.list_node.next.prev = self.header.list_node.prev;
            child_allocator.destroy(self);
        }

        /// Returns true if this superblock is totally full.
        pub fn isFull(self: *const this) bool {
            return self.header.num_free_blocks == 0;
        }

        /// Returns the index of the emptyness bucket that this superblock
        /// should be in -- 0 for totally full, `totally_empty_bucket_index`
        /// for totally empty, or somewhere in between.
        pub fn emptynessBucketIndex(self: *const this) usize {
            if (self.header.num_free_blocks == 0) {
                return 0;  // totally full
            }
            return 1 + @divFloor(params.emptyness_denominator *
                                 self.header.num_free_blocks,
                                 self.header.num_blocks);
        }

        /// Removes this superblock from its current list and places it at the
        /// head of the new list.  If `list` is already the superblock's
        /// current list, then the superblock is moved to the head of that
        /// list.
        pub fn transferTo(self: *this, list: *SuperblockList(Heap)) void {
            // Remove from old list (which may be the same as the new list):
            self.header.list_node.prev.next = self.header.list_node.next;
            self.header.list_node.next.prev = self.header.list_node.prev;
            // Insert into new list:
            self.header.list_node.prev = &list.node;
            self.header.list_node.next = list.node.next;
            list.node.next.prev = &self.header.list_node;
            list.node.next = &self.header.list_node;
        }

        /// Allocates a slice of memory of the given size (which must be no
        /// more than this superblock's block size) from this superblock, or
        /// returns `OutOfMemory` if this superblock is totally full.
        pub fn alloc(self: *this, size: usize) Allocator.Error![]u8 {
            assert(size <= self.header.block_size);
            if (self.header.first_free_block) |ptr| {
                assert(self.header.num_free_blocks > 0);
                self.header.num_free_blocks -= 1;
                self.header.first_free_block =
                    @ptrCast(*?[*]u8,
                             @alignCast(params.min_block_size, ptr)).*;
                var new_mem = ptr[0..size];
                // In debug mode, memset the allocated portion of the block to
                // 0xaa, and the unallocated portion of the block to 0xdd.
                if (builtin.mode == builtin.Mode.Debug) {
                    std.mem.set(u8, new_mem, params.allocated_byte_memset);
                    const start = @ptrToInt(new_mem.ptr) + new_mem.len;
                    const len = self.header.block_size - new_mem.len;
                    std.mem.set(u8, @intToPtr([*]u8, start)[0..len],
                                params.deallocated_byte_memset);
                }
                return new_mem;
            } else {
                assert(self.header.num_free_blocks == 0);
                return Allocator.Error.OutOfMemory;
            }
        }

        /// Frees a slice of memory from this superblock.
        pub fn free(self: *this, old_mem: []u8) void {
            // Check that `old_mem` belongs to this superblock.
            assert(@ptrToInt(old_mem.ptr) & params.superblock_ptr_mask ==
                   @ptrToInt(self));
            // Sanity-check size and alignment of `old_mem`.
            assert(old_mem.len <= self.header.block_size);
            assert(@ptrToInt(old_mem.ptr) % self.header.block_size == 0);
            // Sanity-check that this superblock isn't totally empty (or else
            // how could we be freeing from it?).
            assert(self.header.num_free_blocks < self.header.num_blocks);
            // TODO: In debug builds, walk freelist to check for double-free.
            // Free the block by adding it to the freelist.
            @ptrCast(*?[*]u8, @alignCast(params.min_block_size,
                                         old_mem.ptr)).* =
                self.header.first_free_block;
            self.header.first_free_block = old_mem.ptr;
            self.header.num_free_blocks += 1;
            // In debug mode, memset the rest of the free block to 0xdd.
            if (builtin.mode == builtin.Mode.Debug) {
                const ptr_size = @sizeOf(*?[*]u8);
                const start = @ptrToInt(old_mem.ptr) + ptr_size;
                const len = self.header.block_size - ptr_size;
                std.mem.set(u8, @intToPtr([*]u8, start)[0..len],
                            params.deallocated_byte_memset);
            }
        }
    };
}

pub const SuperblockListNode = packed struct {
    next: *SuperblockListNode,
    prev: *SuperblockListNode,
};

pub fn SuperblockList(comptime Heap: type) type {
    return struct {
        node: SuperblockListNode,

        /// Initializes this list to empty.
        pub fn init(self: *this) void {
            self.node.next = &self.node;
            self.node.prev = &self.node;
        }

        /// Frees all superblocks in this list.
        fn deinit(self: *this, child_allocator: *Allocator) void {
            while (!self.isEmpty()) {
                const header =
                    @fieldParentPtr(SuperblockHeader(Heap), "list_node",
                                    self.node.next);
                const superblock =
                    @fieldParentPtr(Superblock(Heap), "header", header);
                superblock.deinit(child_allocator);
            }
        }

        /// Returns true if the list is empty.
        fn isEmpty(self: *const this) bool {
            assert((self.node.next == &self.node) ==
                   (self.node.prev == &self.node));
            return self.node.next == &self.node;
        }

        /// Returns the superblock at the head of the list, or null if the list
        /// is empty.
        fn head(self: *const this) ?*Superblock(Heap) {
            if (self.isEmpty()) {
                return null;
            } else {
                const header =
                    @fieldParentPtr(SuperblockHeader(Heap), "list_node",
                                    self.node.next);
                assert(header.magic_number == params.superblock_magic_number);
                return @fieldParentPtr(Superblock(Heap), "header", header);
            }
        }
    };
}

//===========================================================================//

test "Superblock struct size is correct" {
    comptime assert(@sizeOf(Superblock(@OpaqueType())) ==
                    params.superblock_size);
}

//===========================================================================//
