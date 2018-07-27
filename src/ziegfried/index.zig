const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ZiegfriedAllocator = struct {
    pub allocator: Allocator,
    child_allocator: &Allocator,

    pub fn init(child_allocator: &Allocator) ZiegfriedAllocator {
        return ZiegfriedAllocator{
            .allocator = Allocator{
                .allocFn = alloc,
                .reallocFn = realloc,
                .freeFn = free,
            },
            .child_allocator = child_allocator,
        };
    }

    fn alloc(allocator: &Allocator, size: usize, alignment: u29) ![]u8 {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        // TODO: Only defer directly to child allocator for large allocations.
        return try self.child_allocator.allocFn(self.child_allocator, size,
                                                alignment);
    }

    fn realloc(allocator: &Allocator, slice: []u8, new_size: usize,
               alignment: u29) ![]u8 {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        // TODO: Only defer directly to child allocator for large allocations.
        return try self.child_allocator.reallocFn(self.child_allocator, slice,
                                                  new_size, alignment);
    }

    fn free(allocator: &Allocator, slice: []u8) void {
        const self = @fieldParentPtr(ZiegfriedAllocator, "allocator",
                                     allocator);
        // TODO: Only defer directly to child allocator for large allocations.
        self.child_allocator.freeFn(self.child_allocator, slice);
    }
};

test "alloc_i32" {
    var buffer: [24]u8 = undefined;
    var buffer_allocator =
        std.heap.FixedBufferAllocator.init(buffer[0..buffer.len]);
    var ziegfried = ZiegfriedAllocator.init(&buffer_allocator.allocator);
    var ptr = try ziegfried.allocator.create(i32);
    *ptr = 12345;
    ziegfried.allocator.destroy(ptr);
}

test "alloc_too_large" {
    var buffer: [24]u8 = undefined;
    var buffer_allocator =
        std.heap.FixedBufferAllocator.init(buffer[0..buffer.len]);
    var ziegfried = ZiegfriedAllocator.init(&buffer_allocator.allocator);
    if (ziegfried.allocator.alignedAlloc(u8, 1, 256)) |_slice| {
        std.debug.assert(false);
    } else |err| {
        std.debug.assert(err == Allocator.Error.OutOfMemory);
    }
}
