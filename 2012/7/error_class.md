# Creating your own error class with `util.inherits` is not that simple

If you're using node.js and want to create a "derived class", you may use util.inherits.

    var util = require('util');

    function Base(message) {
        this.message = message;
    }

    function Derived(message, value) {
        Derived.super_.call(this, message);
        this.value = value;
    }

    util.inherits(Derived, Base);

    var d = new Derived("Test", 1);
    console.log(d.message); // -> "Test"
    console.log(d instanceof Derived); // -> true
    console.log(d instanceof Base); // -> true

Let's create your own error class derived from Error using the same method.

    function MyError(message, value) {
        MyError.super_.call(this, message);
        this.value = value;
    }

    util.inherits(MyError, Error);

    var e = new MyError('Test');
    console.log(e.message); // -> "" !!!!
    console.log(e instanceof MyError); // -> true
    console.log(e instanceof Error); // -> true

But you'll find that message property is not set properly. This is because Error constructor returns a new object if you call it as a function not as a constructor. Error constructor sets message property of the new object but the new object will be discarded.

How about using an object returned by Error?

    function YourError(message, value) {
        var that = YourError.super_.call(this, message);
        that.value = value;
        return that;
    }

    util.inherits(YourError, Error);

    var e = new YourError('Test');
    console.log(e.message); // -> "Test"
    console.log(e instanceof YourError); // -> false !!!!
    console.log(e instanceof Error); // -> true

In this case, message property is set property, but the object is no longer an instance of YourError.

Setting message property manually without calling Error constructor works, but I don't like this way because of the lack of consistency.

    function HisError(message, value) {
        this.message = message;
        this.value = value;
    }

    util.inherits(HisError, Error);

    var e = new HisError('Test');
    console.log(e.message); // -> "Test"
    console.log(e instanceof HisError); // -> true
    console.log(e instanceof Error); // -> true
