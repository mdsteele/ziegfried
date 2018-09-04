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
pub const superblock_ptr_mask = ~(usize(superblock_size) - 1);

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

/// Arbitrary magic number stored as the first field in a Hyperblock struct.
/// We use this to help detect errors where a program tries to free memory to
/// our allocator that was not allocated by it.
pub const hyperblock_magic_number: u32 = 0xda5b76a9;

/// The size of a Hyperblock struct.  We'll typically be allocating these
/// directly from the OS, so it's best for this to be a multiple of the system
/// page size.
pub const hyperblock_size = 8 * std.os.page_size;

/// The number of equal-sized pieces that a hyperblock is divided into.  The
/// first piece holds the hyperblock header, and the rest are used as
/// allocation chunks.
pub const chunk_denominator = 128;

/// The number of chunks in a hyperblock.
pub const hyperblock_num_chunks = chunk_denominator - 1;

/// The size of a hyperblock chunk.
pub const chunk_size = @divExact(hyperblock_size, chunk_denominator);

/// The maxinum number of hyperblock chunks that may comprise an allocated
/// span.
pub const allocated_span_max_num_chunks =
    @divExact(std.os.page_size, chunk_size);

/// The maximum size (in bytes) of an allocated span in a hyperblock.  Note
/// that we must leave room for a pointer to the containing hyperblock.
pub const allocated_span_max_size =
    allocated_span_max_num_chunks * chunk_size - @sizeOf(*@OpaqueType());

//===========================================================================//

fn isPowerOfTwo(value: var) bool {
    return value & (value - 1) == 0;
}

test "params" {
    // Make sure the superblock size is a power of two (so that we can mask out
    // block pointers to get a pointer to the containing superblock):
    comptime assert(isPowerOfTwo(superblock_size));

    // Block sizes must be powers of two, so check that the min and max block
    // sizes are powers of two:
    comptime assert(isPowerOfTwo(min_block_size));
    comptime assert(isPowerOfTwo(max_block_size));

    // We use chunk_size as an alignment for hyperblocks, so it must be a power
    // of two:
    comptime assert(isPowerOfTwo(chunk_size));
}

//===========================================================================//
