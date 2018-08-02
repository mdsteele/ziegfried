const builtin = @import("builtin");
const std = @import("std");

//===========================================================================//

/// A simple spin-lock.
pub const SpinLock = struct {
    // TODO: Instead of just storing 1 for held, we should store the thread ID
    // of the holding thread.  Then we could detect cases like a thread trying
    // to acquire a lock it already holds, or a thread releasing a lock held by
    // another thread, or a thread calling assertHeld on a lock held by a
    // different thread.
    locked: u32, // 0 if unheld, 1 if held

    /// Creates a new lock, initially unheld.
    pub fn init() SpinLock {
        return SpinLock{.locked = 0};
    }

    /// Acquires the lock, blocking if necessary.
    pub fn acquire(self: *this) void {
        // TODO: The AtomicOrder params here (and elsewhere) could probably be
        // relaxed a bit.
        while (@cmpxchgWeak(u32, &self.locked, 0, 1,
                            builtin.AtomicOrder.SeqCst,
                            builtin.AtomicOrder.SeqCst) != null) {}
    }

    /// Releases the lock.  Panics if the lock is not currently held.
    pub fn release(self: *this) void {
        if (@cmpxchgStrong(u32, &self.locked, 1, 0,
                           builtin.AtomicOrder.SeqCst,
                           builtin.AtomicOrder.SeqCst) != null) {
            @panic("releasing unheld SpinLock");
        }
    }

    /// Asserts that the lock is currently held.
    pub fn assertHeld(self: *this) void {
        std.debug.assert(@cmpxchgStrong(u32, &self.locked, 1, 1,
                                        builtin.AtomicOrder.SeqCst,
                                        builtin.AtomicOrder.SeqCst) == null);
    }
};

//===========================================================================//

test "spinlock lock/unlock" {
    var mutex = SpinLock.init();
    mutex.acquire();
    mutex.assertHeld();
    mutex.release();
}

//===========================================================================//
