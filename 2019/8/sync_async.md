# Reusing an algorithm in synchronous and asynchronous functions

Sometimes we want to reuse an algorithm with synchronous and asynchronous functions. Let's take `map` as a simple example.

A naive implementation of `map` will be something like this.

```
function map(a, f) {
    const r = [];
    for (const v of a) {
        r.push(f(v));
    }
    return r;
}

console.log(map([1, 2, 3], x => x * 2));
```

Let's convert convert it to an asynchronous function.

```
async function mapAsync(a, f) {
    const r = [];
    for (const v of a) {
        r.push(await f(v));
    }
    return r;
}

mapAsync([1, 2, 3], async x => x * 2).then(console.log);
```

I just marked the function as `async` and insert `await` when calling `f`. I may use `Promise.all` in actual use cases, but it's not part of this post.

As they're very similar, it'd be nice if we can reuse the implementations. It'll become much cumbersome if the algorithm becomes more complex than `map`.

The first attempt is to use a dummy promise (or a monad).

```
const _ = require('lodash');

function mapM(a, f, lift) {
    return _.reduce(a, (p, v) => {
        return p.then(r => {
            return f(v).then(rv => lift(_.concat(r, [rv])));
        });
    }, lift([]));
}
```

With this `mapM`, you can write `map` and `mapAsync` like these.

```
function map(a, f) {
    class Identity {
        constructor(value) {
            this._value = value;
        }

        then(f) {
            return f(this._value);
        }
    }

    function identity(v) {
        return new Identity(v);
    }

    let ra;
    mapM(a, _.flow(f, identity), identity).then(r => ra = r);
    return ra;
}

console.log(map([1, 2, 3], x => x * 2));
```

```
function mapAsync(a, f) {
    return mapM(a, f, async v => v);
}

mapAsync([1, 2, 3], async x => x * 2).then(console.log);
```

This works, but it's a bit redundant. Another approach would be to use a generator. Let's define a generator function `mapY`.

```
function* mapY(a, f) {
    const r = [];
    for (const v of a) {
        r.push(yield f(v));
    }
    return r;
}
```

This look just like the first version of `mapAsync`, but has `function*` instead of `async function`, and uses `yield` instead of `await`.

Then we can define functions to run `mapY` synchronously or asynchronously.

```
function run(i) {
    function r(v) {
        const x = i.next(v);
        return x.done ? x.value : r(x.value);
    }
    return r();
}

console.log(run(mapY([1, 2, 3], x => x * 2)));
```

```
function runAsync(i) {
    function r(v) {
        const x = i.next(v);
        return x.done ? x.value : x.value.then(r);
    }
    return r();
}

runAsync(mapY([1, 2, 3], async x => x * 2)).then(console.log);
```
