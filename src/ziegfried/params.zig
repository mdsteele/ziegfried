const std = @import("std");
const assert = std.debug.assert;

//===========================================================================//

/// Arbitrary magic number stored as the first field in a Superblock struct.
/// We use this to help detect errors where a program tries to free memory to
/// our allocator that was not allocated by it.
pub const superblock_magic_number: u32 = 0xb40cd14d;

/// The size of a Superblock struct.  We'll typically be allocating these
/// directly from the OS, so it's best for this to be a multiple of the system
/// page size.
pub const superblock_size = std.os.page_size;

/// Bitwise-anding this mask with a pointer to a block will yield the pointer
/// to its containing superblock.  This works because superblocks are always
/// aligned to `superblock_size`.
pub const superblock_ptr_mask: usize = ~(@intCast(usize, superblock_size) - 1);

/// The maximum block size for a superblock.  Allocations larger than this are
/// delegated to the child allocator (which will typically be allocating
/// directly from the OS).
pub const max_block_size = @divExact(superblock_size, 16);

/// The smallest block size for a superblock.  We need this to be at least the
/// size of a pointer to a block so that we can store a freelist within the
/// unallocated blocks in a superblock.
pub const min_block_size = @sizeOf(?[*]u8);

/// Superblocks within a heap are sorted into (emptyness_denominator + 2)
/// different buckets based on how full they are.  Bucket #0 contains
/// totally-full superblocks; bucket #1 contains superblocks that are up to
/// 1/emptyness_denominator empty; bucket #2 contains superblocks that are
/// between 1/emptyness_denominator and 2/emptyness_denominator empty, and so
/// on; and the last bucket contains totally-empty superblocks.
pub const emptyness_denominator = 4;
pub const num_emptyness_buckets = emptyness_denominator + 2;
pub const totally_empty_bucket_index = num_emptyness_buckets - 1;

//===========================================================================//

test "params" {
    // Make sure the superblock size is a power of two (so that we can mask out
    // block pointers to get a pointer to the containing superblock):
    comptime assert(superblock_size & (superblock_size - 1) == 0);

    // Check that the min and max block sizes are powers of two:
    comptime assert(min_block_size & (min_block_size - 1) == 0);
    comptime assert(max_block_size & (max_block_size - 1) == 0);
}

//===========================================================================//
