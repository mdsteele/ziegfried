const builtin = @import("builtin");
const std = @import("std");
const assert = std.debug.assert;

//===========================================================================//

/// List node for an intrusive doubly-linked list.  This struct is packed so
/// that it can be included in other packed structs.
pub const ListNode = packed struct {
    next: *ListNode,
    prev: *ListNode,

    /// Initializes this node to not be linked to any other nodes.
    pub fn init(self: *this) void {
        self.next = self;
        self.prev = self;
    }

    /// Returns true if this node is not linked to any other nodes.
    pub fn isEmpty(self: *const this) bool {
        assert((self.next == self) == (self.prev == self));
        return self.next == self;
    }

    /// Links this node just after another node.  This node must not be
    /// currently linked to any other node.
    pub fn insertAfter(self: *this, other: *this) void {
        assert(self.isEmpty());
        self.prev = other;
        self.next = other.next;
        other.next.prev = self;
        other.next = self;
    }

    /// Removes this node from its current list (if any), making it no longer
    /// linked to any other node.
    pub fn remove(self: *this) void {
        self.prev.next = self.next;
        self.next.prev = self.prev;
        self.next = self;
        self.prev = self;
    }
};

//===========================================================================//

test "ListNode.init() makes an empty node" {
    var node: ListNode = undefined;
    node.init();
    assert(node.isEmpty());
}

test "ListNode insert and remove" {
    var node1: ListNode = undefined;
    node1.init();
    var node2: ListNode = undefined;
    node2.init();
    node1.insertAfter(&node2);
    assert(!node1.isEmpty());
    assert(!node2.isEmpty());
    node1.remove();
    assert(node1.isEmpty());
    assert(node2.isEmpty());
}

//===========================================================================//
