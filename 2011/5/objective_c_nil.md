# Sending a message to nil may return an undefined struct

As described at [Sending message to nil](http://developer.apple.com/library/ios/#documentation/Cocoa/Conceptual/ObjectiveC/Chapters/ocObjectsClasses.html#//apple_ref/doc/uid/TP30001163-CH11-SW7), when you send a message to `nil`, it may return an struct filled with 0.0, or an undefined struct. It depends whether the struct is returned in registers or not.

So you may not write code like this

    UIView *view = object.view; // May be nil
    CGRect frame = view.frame;

Even though frame is `{ { 0.0, 0.0 }, { 0.0, 0.0 } }` in most cases when view is nil, but it's not necessarily so. You should not depend on this behavior.
