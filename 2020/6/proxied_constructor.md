# Extend a class with a proxied constructor

I was working on migrating [sequelize](https://sequelize.org/) 4 to 5 when I found a strange behavior. We have a custom data type derived from `STRING`. It's something like this.

```
class CUSTOM_STRING extends DataTypes.STRING {
    toSql() {
        return ...;
    }
    ...
}
```

Although this worked fine with sequelize 4, I found it no longer worked with sequelize 5. The thing is, this overridden `toSql` was no longer called even when I created an instance of this class and call `toSql` on it.

```
const s = new CUSTOM_STRING();
s.toSql(); // This calls STRING.toSql instead of CUSTOM_STRING.toSql
```

After looking into sequelize, I found that all data type classes are wrapped in a proxy using [classToInvokable](https://github.com/sequelize/sequelize/blob/master/lib/utils/class-to-invokable.js).

I wrote a simple code that demonstrates this issue.

```
class X {
    name() { return 'X'; }
}

const XP = new Proxy(X, {
    apply(Target, thisArg, args) { return new Target(...args); },
    construct(Target, args) { return new Target(...args); },
    get(target, p) { return target[p]; }
});

class Y extends XP {
    name() { return "Y"; }
}

const y = new Y();
console.log(y.name());
```

This prints `X` instead of `Y`. The reason is that this proxy has `construct` handler and it returns a new object of `Target`. This handler will be called when the constructor of `Y` calls `super()` implicitly and it'll sets `this` to an object returned by the proxy handler.

```
class Y extends XP {
    constructor() {
        super();
        console.log(this);
    }
}
```

You'll find this prints `X {}` instead of `Y {}`. Even though you `new`ed `Y`, it returned an instance of `X`.

To workaround this, you can extend `Y` from `XP.prototype.constructor` instead of `XP` (Note that `X` itself is invisible in the sequelize case). This works because a prototype of a class is the class itself and the proxy's `get` handler just returns a target's property.

```
class Y extends XP.prototype.constructor {
    name() { return "Y"; }
}

const y = new Y();
console.log(y.name());
```

This prints `Y` as you'd expect.
