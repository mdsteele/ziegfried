// TODO: Right now, this allocator is not thread-safe.  We should make it
// thread-safe, or add an alternate implementation that is thread-safe.

// TODO: Right now, this allocator only handles small allocations (at most 256
// bytes or so, which are the most common case for many programs) and defers
// all others to the child allocator.  If the child allocator is
// DirectAllocator, this will mean that medium-sized allocations (e.g. 1024
// bytes) are very inefficient.  The plan is for this library to provide
// another allocator layer that fits between this small-block allocator and
// DirectAllocator for handling medium-sized allocations.

const std = @import("std");
const assert = std.debug.assert;
const assertError = std.debug.assertError;
const Allocator = std.mem.Allocator;

//===========================================================================//

/// Arbitrary magic number stored as the first field in a Superblock struct.
/// We use this to help detect errors where a program tries to free memory to
/// our allocator that was not allocated by it.
const superblock_magic_number: u32 = 0xb40cd14d;

/// The size of a Superblock struct.  We'll typically be allocating these
/// directly from the OS, so it's best for this to be a multiple of the system
/// page size.
const superblock_total_size = std.os.page_size;

/// The number of bytes within each superblock that are available to be
/// allocated.  The number of blocks in a superblock with a given block size is
/// equal to `@divFloor(superblock_alloc_space, block_size)`.
const superblock_alloc_space =
    superblock_total_size - @sizeOf(SuperblockHeader);

/// Bitwise-anding this mask with a pointer to a block will yield the pointer
/// to its containing superblock.  This works because superblocks are always
/// aligned to `superblock_total_size`.
const superblock_ptr_mask: usize =
    ~(@intCast(usize, superblock_total_size) - 1);

/// The maximum block size for a superblock.  Allocations larger than this are
/// delegated to the child allocator (which will typically be allocating
/// directly from the OS).
const max_block_size = @divExact(superblock_total_size, 16);

/// The smallest block size for a superblock.  We need this to be at least the
/// size of a pointer to a block so that we can store a freelist within the
/// unallocated blocks in a superblock.
const min_block_size = @sizeOf(?[*]u8);

/// How many distinct block sizes there are.  Right now, all block sizes are
/// powers of 2, from min_block_size to max_block_size inclusive.
const num_block_sizes = std.math.log2_int_ceil(usize, max_block_size) -
    std.math.log2_int_ceil(usize, min_block_size) + 1;

/// Superblocks within a heap are sorted into (emptyness_denominator + 2)
/// different buckets based on how full they are.  Bucket #0 contains
/// totally-full superblocks; bucket #1 contains superblocks that are up to
/// 1/emptyness_denominator empty; bucket #2 contains superblocks that are
/// between 1/emptyness_denominator and 2/emptyness_denominator empty, and so
/// on; and the last bucket contains totally-empty superblocks.
const emptyness_denominator = 4;
const num_emptyness_buckets = emptyness_denominator + 2;
const totally_empty_bucket_index = num_emptyness_buckets - 1;

test "constants" {
    // Make sure the superblock size is correct:
    comptime assert(@sizeOf(Superblock) == superblock_total_size);

    // Make sure the superblock size is a power of two (so that we can mask out
    // block pointers to get a pointer to the containing superblock):
    comptime assert(superblock_total_size & (superblock_total_size - 1) == 0);

    // Check that the min and max block sizes are powers of two:
    comptime assert(min_block_size & (min_block_size - 1) == 0);
    comptime assert(max_block_size & (max_block_size - 1) == 0);

    // Sanity-check that there is room in a superblock to allocate at least one
    // of the largest block size.
    comptime assert(max_block_size <= superblock_alloc_space);
}

//===========================================================================//

// The Superblock struct is kept packed so that we can rely on the alignments
// of positions within its `blocks` array.  This in turn obligates us to
// declare SuperblockHeader and SuperblockListNode packed as well.

const SuperblockHeader = packed struct {
    /// This field always stores superblock_magic_number.
    magic_number: u32,
    /// The block size for this superblock.
    block_size: u32,
    /// The number of blocks in this superblock.  This is equal to
    /// @divFloor(superblock_alloc_space, block_size).
    num_blocks: u32,
    /// The number of blocks in this superblock that aren't currently
    /// allocated.
    num_free_blocks: u32,
    /// The head of the freelist.
    first_free_block: ?[*]u8,
    /// The heap that this superblock belongs to.
    heap: *Heap,
    /// Intrusive doubly-linked list node.
    list_node: SuperblockListNode,
};

const Superblock = packed struct {
    header: SuperblockHeader,
    blocks: [superblock_alloc_space]u8,

    fn init(heap: *Heap,
            child_allocator: *Allocator) !*Superblock {
        const block_size = heap.block_size;
        assert(block_size >= min_block_size);
        assert(block_size <= max_block_size);
        // Allocate a new superblock from the child allocator:
        var superblocks = try child_allocator.alignedAlloc(
            Superblock, superblock_total_size, 1);
        var superblock = &superblocks[0];
        assert(@ptrToInt(superblock) % superblock_total_size == 0);
        // Initialize bookkeeping header values:
        superblock.header.magic_number = superblock_magic_number;
        superblock.header.block_size = block_size;
        superblock.header.num_blocks =
            @divFloor(superblock_alloc_space, block_size);
        superblock.header.num_free_blocks = superblock.header.num_blocks;
        superblock.header.heap = heap;
        // Initialize block free list:
        var next: ?[*]u8 = null;
        var offset: usize = superblock_alloc_space;
        while (offset >= block_size) {
            offset -= block_size;
            const ptr: [*]u8 = superblock.blocks[offset..].ptr;
            @ptrCast(*?[*]u8, @alignCast(min_block_size, ptr)).* = next;
            next = ptr;
        }
        superblock.header.first_free_block = next;
        // Insert into superblock list:
        const insert_after =
            &heap.emptyness_buckets[totally_empty_bucket_index].node;
        superblock.header.list_node.prev = insert_after;
        superblock.header.list_node.next = insert_after.next;
        insert_after.next.prev = &superblock.header.list_node;
        insert_after.next = &superblock.header.list_node;
        return superblock;
    }

    fn deinit(self: *Superblock, child_allocator: *Allocator) void {
        self.header.list_node.prev.next = self.header.list_node.next;
        self.header.list_node.next.prev = self.header.lsit_node.prev;
        child_allocator.free(self);
    }

    /// Returns true if this superblock is totally full.
    fn isFull(self: *Superblock) bool {
        return self.header.num_free_blocks == 0;
    }

    /// Returns the index of the emptyness bucket that this superblock should
    /// be in -- 0 for totally full, `totally_empty_bucket_index` for totally
    /// empty, or somewhere in between.
    fn emptynessBucketIndex(self: *Superblock) usize {
        if (self.header.num_free_blocks == 0) {
            return 0;  // totally full
        }
        return 1 + @divFloor(emptyness_denominator *
                             self.header.num_free_blocks,
                             self.header.num_blocks);
    }

    /// Removes this superblock from its current list and places it at the head
    /// of the new list.  If `list` is already the superblock's current list,
    /// then the superblock is moved to the head of that list.
    fn transferTo(self: *Superblock, list: *SuperblockList) void {
        // Remove from old list (which may be the same as the new list):
        self.header.list_node.prev.next = self.header.list_node.next;
        self.header.list_node.next.prev = self.header.list_node.prev;
        // Insert into new list:
        self.header.list_node.prev = &list.node;
        self.header.list_node.next = list.node.next;
        list.node.next.prev = &self.header.list_node;
        list.node.next = &self.header.list_node;
    }

    /// Allocates a slice of memory of the given size (which must be no more
    /// than this superblock's block size) from this superblock, or returns
    /// `OutOfMemory` if this superblock is totally full.  This should only be
    /// called from `Heap.allocFrom`.
    fn alloc(self: *Superblock, size: usize) Allocator.Error![]u8 {
        assertMsg(size <= self.header.block_size,
                  "cannot alloc size {} in superblock with block size {}\n",
                  size, self.header.block_size);
        if (self.header.first_free_block) |ptr| {
            assertMsg(self.header.num_free_blocks > 0,
                      "has first_free_block, but num_free_blocks is {}\n",
                      self.header.num_free_blocks);
            self.header.num_free_blocks -= 1;
            self.header.first_free_block =
                @ptrCast(*?[*]u8, @alignCast(min_block_size, ptr)).*;
            return ptr[0..size];
        } else {
            assertMsg(self.header.num_free_blocks == 0,
                      "no first_free_block, but num_free_blocks is {}\n",
                      self.header.num_free_blocks);
            return Allocator.Error.OutOfMemory;
        }
    }

    /// Frees a slice of memory from this superblock.  This should only be
    /// called from `Heap.freeFrom`.
    fn free(self: *Superblock, old_mem: []u8) void {
        // Check that `old_mem` belongs to this superblock.
        assert(@ptrToInt(old_mem.ptr) & superblock_ptr_mask ==
               @ptrToInt(self));
        // Sanity-check size and alignment of `old_mem`.
        assert(old_mem.len <= self.header.block_size);
        assert(@ptrToInt(old_mem.ptr) % self.header.block_size == 0);
        // Sanity-check that this superblock isn't totally empty (or else how
        // could we be freeing from it?).
        assert(self.header.num_free_blocks < self.header.num_blocks);
        // TODO: In debug builds, walk freelist to check for double-free.
        // Free the block by adding it to the freelist.
        @ptrCast(*?[*]u8, @alignCast(min_block_size, old_mem.ptr)).* =
            self.header.first_free_block;
        self.header.first_free_block = old_mem.ptr;
        self.header.num_free_blocks += 1;
    }
};

const SuperblockListNode = packed struct {
    next: *SuperblockListNode,
    prev: *SuperblockListNode,
};

//===========================================================================//

const Heap = struct {
    /// The block size of superblocks in this heap.
    block_size: u32,
    /// Array of doubly-linked lists of superblocks in various states of
    /// emptyness.
    emptyness_buckets: [num_emptyness_buckets]SuperblockList,

    fn init(self: *Heap, block_size: u32) void {
        self.block_size = block_size;
        for (self.emptyness_buckets) |*bucket| {
            bucket.init();
            assert(bucket.isEmpty());
        }
    }

    fn deinit(self: *Heap, child_allocator: *Allocator) void {
        for (self.emptyness_buckets) |bucket| {
            bucket.deinit(child_allocator);
        }
    }

    /// Allocates a slice of memory of the given size (which must be no more
    /// than this heap's block size) from this heap; returns `OutOfMemory` if
    /// all superblocks in the heap are totally full and we fail to allocate a
    /// new superblock from the child allocator.
    fn alloc(self: *Heap, size: usize,
             child_allocator: *Allocator) Allocator.Error![]u8 {
        assert(size <= self.block_size);
        // Check each bucket from most to least full (skipping bucket #0, since
        // that's the totally-full superblocks).
        assert(if (self.emptyness_buckets[0].head()) |superblock|
                 superblock.isFull() else true);
        for (self.emptyness_buckets[1..]) |*bucket, i| {
            const old_index = i + 1;
            if (bucket.head()) |superblock| {
                return try self.allocFrom(superblock, old_index, size,
                                          child_allocator);
            }
        }
        // All superblocks are full, so allocate a new one.
        const superblock = try Superblock.init(self, child_allocator);
        assert(superblock.emptynessBucketIndex() ==
               totally_empty_bucket_index);
        return try self.allocFrom(superblock, totally_empty_bucket_index, size,
                                  child_allocator);
    }

    /// Allocates a slice of memory from the given superblock within this heap.
    /// `old_index` must be the superblock's current emptyness bucket index.
    fn allocFrom(self: *Heap, superblock: *Superblock,
                 old_index: usize, size: usize,
                 child_allocator: *Allocator) Allocator.Error![]u8 {
        assert(superblock.header.heap == self);
        const mem = try superblock.alloc(size);
        const new_index = superblock.emptynessBucketIndex();
        if (new_index != old_index) {
            superblock.transferTo(&self.emptyness_buckets[new_index]);
        }
        return mem;
    }

    /// Frees a slice of memory from the given superblock within this heap.
    fn freeFrom(self: *Heap, superblock: *Superblock, old_mem: []u8) void {
        assert(superblock.header.heap == self);
        superblock.free(old_mem);
        const index = superblock.emptynessBucketIndex();
        if (index == totally_empty_bucket_index) {
            // TODO: Under certain conditions, free the now-totally-empty
            // superblock back to the child allocator here.
        }
        // Move the superblock to its new emptyness bucket (or to the front of
        // its same emptyness bucket).
        superblock.transferTo(&self.emptyness_buckets[index]);
    }
};

const SuperblockList = struct {
    node: SuperblockListNode,

    /// Initializes this list to empty.
    fn init(self: *SuperblockList) void {
        self.node.next = &self.node;
        self.node.prev = &self.node;
    }

    /// Frees all superblocks in this list.
    fn deinit(self: *SuperblockList, child_allocator: *Allocator) void {
        while (!self.isEmpty()) {
            var superblock =
                @fieldParentPtr(Superblock, "list_node", self.node.next);
            superblock.deinit(child_allocator);
        }
    }

    /// Returns true if the list is empty.
    fn isEmpty(self: *const SuperblockList) bool {
        assert((self.node.next == &self.node) ==
               (self.node.prev == &self.node));
        return self.node.next == &self.node;
    }

    /// Returns the superblock at the head of the list, or null if the list is
    /// empty.
    fn head(self: *const SuperblockList) ?*Superblock {
        if (self.isEmpty()) {
            return null;
        } else {
            const header = @fieldParentPtr(SuperblockHeader, "list_node",
                                           self.node.next);
            assert(header.magic_number == superblock_magic_number);
            return @fieldParentPtr(Superblock, "header", header);
        }
    }
};

//===========================================================================//

pub const ZiegfriedAllocator = struct {
    pub allocator: Allocator,
    child_allocator: *Allocator,
    heaps: []Heap,

    pub fn init(child_allocator: *Allocator) !ZiegfriedAllocator {
        const num_heaps = num_block_sizes;
        var heaps = try child_allocator.alloc(Heap, num_heaps);
        var block_size: u32 = min_block_size;
        for (heaps) |*heap| {
            heap.init(block_size);
            block_size <<= 1;
        }
        return ZiegfriedAllocator{
            .allocator = Allocator{
                .allocFn = alloc,
                .reallocFn = realloc,
                .freeFn = free,
            },
            .child_allocator = child_allocator,
            .heaps = heaps,
        };
    }

    pub fn deinit(self: *ZiegfriedAllocator) void {
        for (heaps) |heap| {
            heap.deinit(self.child_allocator);
        }
        self.child_allocator.free(self.heaps);
    }

    fn alloc(allocator: *Allocator, size: usize, alignment: u29) ![]u8 {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        if (size > max_block_size or alignment > max_block_size) {
            return try self.child_allocator.allocFn(self.child_allocator, size,
                                                    alignment);
        }
        const virtual_size =
            std.math.max(std.math.max(size, min_block_size), alignment);
        const block_size_exponent =
            std.math.log2_int_ceil(usize, virtual_size);
        assert(block_size_exponent >=
               std.math.log2_int_ceil(usize, min_block_size));
        assert(block_size_exponent <=
               std.math.log2_int_ceil(usize, max_block_size));
        const block_size_index = block_size_exponent -
            std.math.log2_int_ceil(usize, min_block_size);
        const heap = &self.heaps[block_size_index];
        return try heap.alloc(size, self.child_allocator);
    }

    fn realloc(allocator: *Allocator, old_mem: []u8, new_size: usize,
               alignment: u29) ![]u8 {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        // If the old memory is large enough that it came directly from the
        // child allocator, and the new size/alignment also needs to be handled
        // directly by the child allocator, then we can just let the child
        // allocator handle the whole realloc.
        if (old_mem.len > max_block_size and
            (new_size > max_block_size or alignment > max_block_size)) {
            return try self.child_allocator.reallocFn(self.child_allocator,
                                                      old_mem, new_size,
                                                      alignment);
        }
        @panic("TODO implement realloc");
    }

    fn free(allocator: *Allocator, old_mem: []u8) void {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        if (old_mem.len > max_block_size) {
            return self.child_allocator.freeFn(self.child_allocator, old_mem);
        }
        const superblock: *Superblock =
            @intToPtr(*Superblock,
                      @ptrToInt(old_mem.ptr) & superblock_ptr_mask);
        if (superblock.header.magic_number != superblock_magic_number) {
            @panic("Tried to free memory not allocated by this allocator");
        }
        superblock.header.heap.freeFrom(superblock, old_mem);
    }
};

//===========================================================================//

fn assertMsg(condition: bool, comptime format: []const u8, args: ...) void {
    if (!condition) {
        std.debug.panic(format, args);
    }
}

//===========================================================================//

test "createOne(i32)" {
    var buffer: [1 << 16]u8 = undefined;
    var buffer_allocator = std.heap.FixedBufferAllocator.init(buffer[0..]);
    var ziegfried = try ZiegfriedAllocator.init(&buffer_allocator.allocator);
    var ptr = try ziegfried.allocator.createOne(i32);
    ptr.* = 12345;
    ziegfried.allocator.destroy(ptr);
}

test "huge allocation, out of memory" {
    var buffer: [1 << 16]u8 = undefined;
    var buffer_allocator = std.heap.FixedBufferAllocator.init(buffer[0..]);
    var ziegfried = try ZiegfriedAllocator.init(&buffer_allocator.allocator);
    assertError(ziegfried.allocator.alignedAlloc(u8, 1, 1 << 20),
                Allocator.Error.OutOfMemory);
}

test "allocate a bunch of small objects" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);

    var slice = try ziegfried.allocator.alloc(*i32, 50);
    defer ziegfried.allocator.free(slice);
    assert(slice.len == 50);
    for (slice) |*item, i| {
        item.* = try ziegfried.allocator.create(@intCast(i32, i));
    }
    for (slice) |item, i| {
        assert(item.* == @intCast(i32, i));
        ziegfried.allocator.destroy(item);
    }
}

test "allocate many differently-sized objects" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);

    var prng = std.rand.DefaultPrng.init(12345);
    var slice = try ziegfried.allocator.alloc([]u8, 10000);
    defer ziegfried.allocator.free(slice);
    for (slice) |*item| {
        const size = prng.random.range(u8, 1, 250);
        item.* = try ziegfried.allocator.alloc(u8, size);
        assert(item.len == size);
        for (item.*) |*byte| byte.* = size;
    }

    prng.random.shuffle([]u8, slice);
    for (slice) |item| {
        assert(item.len <= 250);
        const size = @intCast(u8, item.len);
        for (item) |byte| assert(byte == size);
        ziegfried.allocator.free(item);
    }
}

// TODO: Add lots more tests!

//===========================================================================//
