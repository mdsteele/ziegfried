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

const params = @import("params.zig");

const sblock = @import("sblock.zig");
const Superblock = sblock.Superblock(STHeap);
const SuperblockList = sblock.SuperblockList(STHeap);

//===========================================================================//

/// How many distinct block sizes there are.  Right now, all block sizes are
/// powers of 2, from params.min_block_size to params.max_block_size inclusive.
const num_block_sizes = std.math.log2_int_ceil(usize, params.max_block_size) -
    std.math.log2_int_ceil(usize, params.min_block_size) + 1;

//===========================================================================//

/// A heap for a single-threaded allocator.
const STHeap = struct {
    /// The block size of superblocks in this heap.
    block_size: u32,
    /// Array of doubly-linked lists of superblocks in various states of
    /// emptyness.
    emptyness_buckets: [params.num_emptyness_buckets]SuperblockList,

    fn init(self: *this, block_size: u32) void {
        self.block_size = block_size;
        for (self.emptyness_buckets) |*bucket| {
            bucket.init();
            assert(bucket.isEmpty());
        }
    }

    fn deinit(self: *this, child_allocator: *Allocator) void {
        for (self.emptyness_buckets) |bucket| {
            bucket.deinit(child_allocator);
        }
    }

    /// Allocates a slice of memory of the given size (which must be no more
    /// than this heap's block size) from this heap; returns `OutOfMemory` if
    /// all superblocks in the heap are totally full and we fail to allocate a
    /// new superblock from the child allocator.
    fn alloc(self: *this, size: usize,
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
        const superblock =
            try Superblock.init(self.block_size, self, child_allocator);
        assert(superblock.emptynessBucketIndex() ==
               params.totally_empty_bucket_index);
        superblock.transferTo(
            &self.emptyness_buckets[params.totally_empty_bucket_index]);
        return try self.allocFrom(
            superblock, params.totally_empty_bucket_index, size,
            child_allocator);
    }

    /// Allocates a slice of memory from the given superblock within this heap.
    /// `old_index` must be the superblock's current emptyness bucket index.
    fn allocFrom(self: *this, superblock: *Superblock,
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
    fn freeFrom(self: *this, superblock: *Superblock,
                old_mem: []u8) void {
        assert(superblock.header.heap == self);
        superblock.free(old_mem);
        const index = superblock.emptynessBucketIndex();
        if (index == params.totally_empty_bucket_index) {
            // TODO: Under certain conditions, free the now-totally-empty
            // superblock back to the child allocator here.
        }
        // Move the superblock to its new emptyness bucket (or to the front of
        // its same emptyness bucket).
        superblock.transferTo(&self.emptyness_buckets[index]);
    }
};

//===========================================================================//

/// A single-threaded (that is, non-thread-safe) allocator.
pub const ZiegfriedAllocator = struct {
    pub allocator: Allocator,
    child_allocator: *Allocator,
    heaps: []STHeap,

    pub fn init(child_allocator: *Allocator) !ZiegfriedAllocator {
        const num_heaps = num_block_sizes;
        var heaps = try child_allocator.alloc(STHeap, num_heaps);
        var block_size: u32 = params.min_block_size;
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
        assert(alignment <= size);
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        if (size > params.max_block_size) {
            return try self.child_allocator.allocFn(self.child_allocator, size,
                                                    alignment);
        }
        const heap = &self.heaps[blockSizeIndex(size)];
        return try heap.alloc(size, self.child_allocator);
    }

    fn realloc(allocator: *Allocator, old_mem: []u8, new_size: usize,
               alignment: u29) ![]u8 {
        assert(alignment <= new_size);
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        const old_size = old_mem.len;
        if (old_size > params.max_block_size) {
            // The old memory is large enough that it came directly from the
            // child allocator.
            if (new_size > params.max_block_size) {
                // The new size also needs to be handled directly by the child
                // allocator, so we can just let the child allocator handle the
                // whole realloc.
                return try self.child_allocator.reallocFn(
                    self.child_allocator, old_mem, new_size, alignment);
            } else {
                // The new size is small enough that we should allocate it from
                // a superblock.
                assert(new_size < old_size);
                const heap = &self.heaps[blockSizeIndex(new_size)];
                // TODO: This allocation can fail, but realloc isn't supposed
                // to fail when new_size <= old_size.  For discussion on this,
                // see https://github.com/ziglang/zig/issues/1306.
                var new_mem = try heap.alloc(new_size, self.child_allocator);
                std.mem.copy(u8, new_mem, old_mem[0..new_size]);
                self.child_allocator.freeFn(self.child_allocator, old_mem);
                return new_mem;
            }
        } else {
            // The old memory is small enough that it should be stored in a
            // superblock; start by sanity-checking that this is the case.
            const superblock: *Superblock =
                @intToPtr(*Superblock,
                          @ptrToInt(old_mem.ptr) & params.superblock_ptr_mask);
            if (superblock.header.magic_number !=
                params.superblock_magic_number) {
                @panic("Tried to realloc memory from a different allocator");
            }
            assert(old_size <= superblock.header.block_size);
            const old_heap = superblock.header.heap;
            assert(old_heap.block_size == superblock.header.block_size);
            if (new_size > params.max_block_size) {
                // The new size is large enough that it must be handled by the
                // child allocator.
                assert(new_size > old_size);
                var new_mem = try self.child_allocator.allocFn(
                    self.child_allocator, new_size, alignment);
                std.mem.copy(u8, new_mem, old_mem);
                old_heap.freeFrom(superblock, old_mem);
                return new_mem;
            } else {
                const new_heap = &self.heaps[blockSizeIndex(new_size)];
                assert(new_size <= new_heap.block_size);
                if (new_heap == old_heap) {
                    // We don't need to be in a different superblock, so we can
                    // just use the same block we're using now.
                    assert(new_size <= superblock.header.block_size);
                    return old_mem.ptr[0..new_size];
                } else if (new_heap.block_size > old_heap.block_size) {
                    // We need a larger block size.
                    assert(new_size > superblock.header.block_size);
                    assert(new_size > old_size);
                    var new_mem =
                        try new_heap.alloc(new_size, self.child_allocator);
                    std.mem.copy(u8, new_mem, old_mem);
                    old_heap.freeFrom(superblock, old_mem);
                    return new_mem;
                } else {
                    // The new size will fit in a smaller block size.  Try to
                    // allocate a smaller block, but if that fails, we can just
                    // reuse the old block.
                    assert(new_heap.block_size < old_heap.block_size);
                    assert(new_size < old_size);
                    var new_mem =
                        new_heap.alloc(new_size, self.child_allocator) catch
                        return old_mem.ptr[0..new_size];
                    std.mem.copy(u8, new_mem, old_mem[0..new_size]);
                    old_heap.freeFrom(superblock, old_mem);
                    return new_mem;
                }
            }
        }
    }

    fn free(allocator: *Allocator, old_mem: []u8) void {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        if (old_mem.len > params.max_block_size) {
            return self.child_allocator.freeFn(self.child_allocator, old_mem);
        }
        const superblock: *Superblock =
            @intToPtr(*Superblock,
                      @ptrToInt(old_mem.ptr) & params.superblock_ptr_mask);
        if (superblock.header.magic_number != params.superblock_magic_number) {
            @panic("Tried to free memory from a different allocator");
        }
        superblock.header.heap.freeFrom(superblock, old_mem);
    }

    fn blockSizeIndex(size: usize) usize {
        const virtual_size = std.math.max(size, params.min_block_size);
        const block_size_exponent =
            std.math.log2_int_ceil(usize, virtual_size);
        assert(block_size_exponent >=
               std.math.log2_int_ceil(usize, params.min_block_size));
        assert(block_size_exponent <=
               std.math.log2_int_ceil(usize, params.max_block_size));
        return block_size_exponent -
            std.math.log2_int_ceil(usize, params.min_block_size);
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

test "realloc from huge to huge" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(u8, 15000);
    assertMsg(old_slice.len == 15000, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = @intCast(u8, i % 256);
    var new_slice = try ziegfried.allocator.realloc(u8, old_slice, 10000);
    assertMsg(new_slice.len == 10000, "new_slice.len = {}", old_slice.len);
    for (new_slice) |value, i| assert(value == i % 256);
    ziegfried.allocator.free(new_slice);
}

test "realloc from huge to small" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(u8, 10000);
    assertMsg(old_slice.len == 10000, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = @intCast(u8, i % 256);
    var new_slice = try ziegfried.allocator.realloc(u8, old_slice, 50);
    assertMsg(new_slice.len == 50, "new_slice.len = {}", old_slice.len);
    assert(new_slice.ptr != old_slice.ptr);
    for (new_slice) |value, i| assert(value == i);
    ziegfried.allocator.free(new_slice);
}

test "realloc from small to huge" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(u8, 50);
    assertMsg(old_slice.len == 50, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = @intCast(u8, i);
    var new_slice = try ziegfried.allocator.realloc(u8, old_slice, 10000);
    assertMsg(new_slice.len == 10000, "new_slice.len = {}", old_slice.len);
    assert(new_slice.ptr != old_slice.ptr);
    for (new_slice[0..50]) |value, i| assert(value == i);
    ziegfried.allocator.free(new_slice);
}

test "realloc smaller within same block size" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(usize, 7);
    assertMsg(old_slice.len == 7, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = i;
    var new_slice = try ziegfried.allocator.realloc(usize, old_slice, 5);
    assertMsg(new_slice.len == 5, "new_slice.len = {}", old_slice.len);
    assertMsg(new_slice.ptr == old_slice.ptr, "old ptr = {}, new ptr = {}",
              old_slice.ptr, new_slice.ptr);
    for (new_slice) |value, i| assert(value == i);
    ziegfried.allocator.free(new_slice);
}

test "realloc bigger within same block size" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(usize, 5);
    assertMsg(old_slice.len == 5, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = i;
    var new_slice = try ziegfried.allocator.realloc(usize, old_slice, 7);
    assertMsg(new_slice.len == 7, "new_slice.len = {}", old_slice.len);
    assertMsg(new_slice.ptr == old_slice.ptr, "old ptr = {}, new ptr = {}",
              old_slice.ptr, new_slice.ptr);
    for (new_slice[0..5]) |value, i| assert(value == i);
    ziegfried.allocator.free(new_slice);
}

test "realloc to a bigger block size" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(u8, 30);
    assertMsg(old_slice.len == 30, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = @intCast(u8, i);
    var new_slice = try ziegfried.allocator.realloc(u8, old_slice, 100);
    assertMsg(new_slice.len == 100, "new_slice.len = {}", old_slice.len);
    assert(new_slice.ptr != old_slice.ptr);
    for (new_slice[0..30]) |value, i| assert(value == i);
    ziegfried.allocator.free(new_slice);
}

test "realloc to a smaller block size" {
    var direct = std.heap.DirectAllocator.init();
    var ziegfried = try ZiegfriedAllocator.init(&direct.allocator);
    var old_slice = try ziegfried.allocator.alloc(u8, 100);
    assertMsg(old_slice.len == 100, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = @intCast(u8, i);
    var new_slice = try ziegfried.allocator.realloc(u8, old_slice, 30);
    assertMsg(new_slice.len == 30, "new_slice.len = {}", old_slice.len);
    assert(new_slice.ptr != old_slice.ptr);
    for (new_slice) |value, i| assert(value == i);
    ziegfried.allocator.free(new_slice);
}

test "realloc to a smaller block size with OOM" {
    // Make the child allocator have just enough memory for the heap array and
    // one superblock.
    var direct = std.heap.DirectAllocator.init();
    var buffer = try direct.allocator.alignedAlloc(
        u8, params.superblock_size, 2 * params.superblock_size);
    defer direct.allocator.free(buffer);
    var buffer_allocator = std.heap.FixedBufferAllocator.init(buffer[0..]);
    var ziegfried = try ZiegfriedAllocator.init(&buffer_allocator.allocator);
    // Allocate a block, which will create a new superblock.
    var old_slice = try ziegfried.allocator.alloc(u8, 100);
    assertMsg(old_slice.len == 100, "old_slice.len = {}", old_slice.len);
    for (old_slice) |*value, i| value.* = @intCast(u8, i);
    // Verify that we cannot create a new smaller block (because the child
    // allocator doesn't have room for another superblock).
    assertError(ziegfried.allocator.alloc(u8, 30),
                Allocator.Error.OutOfMemory);
    // Now reallac the first block to a smaller block size.  Since we can't
    // create a new superblock, as a fallback it should just give us back the
    // same block, trimmed to the smaller size.
    var new_slice = try ziegfried.allocator.realloc(u8, old_slice, 30);
    assertMsg(new_slice.len == 30, "new_slice.len = {}", old_slice.len);
    assert(new_slice.ptr == old_slice.ptr);
    ziegfried.allocator.free(new_slice);
}

//===========================================================================//
