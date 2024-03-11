# Out parameter in Objective-C with ARC

When you use an out parameter in Objective-C with ARC, you should know what's going on behind the scene. Imagine you write a method like this:

    - (void)test:(NSString **)value {
      *value = @"Test";
    }

And call this method like:

    NSString *s = nil;
    [obj test:&s];

It works fine, but you should know that a compiler generates a temporary variable before calling the method. This is because the actual type of the out parameter is `NSString *__autoreleasing*` but the actual variable type is `NSString *__strong`. A compiler generates code like this:

    - (void)test:(NSString *__autoreleasing *)value {
      *value = @"Test";
    }

    {
      ...
      NSString *__strong s = nil;
      NSString *__autoreleasing temp = s;
      [obj test:&temp];
      s = temp;
    }

Now, you're writing a method that returns a block in which an out parameter is set. For example:

    typedef void (^Block)(NSString *);
    - (Block)test:(NSString **)value {
      return ^(NSString *s) {
        *value = s;
      }
    }

and call this like:

    NSString *s = nil;
    Block block = [o test:&s];
    block(@"Test");

Although you may think `s` becomes @"Test", but it doesn't. A code compiler generates is something like:

    NSString *__strong s = nil;
    NSString *__autoreleasing temp = s;
    Block block = [o test:&temp];
    s = temp;
    block(@"Test");

and it's clear that calling `block` won't change a value of `s`. To make it work, you should make the parameter __strong.

    typedef void (^Block)(NSString *);
    - (Block)test:(NSString *__strong *)value {
      return ^(NSString *s) {
        *value = s;
      }
    }

In this case, a compiler won't generate a temporary variable and `s` will be set as you expect.
