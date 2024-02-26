# Type-safe key and value pair

Imagine you have an interface `I`.

```
interface I {
  readonly x: string;
  readonly y: number;
  readonly z: boolean;
}
```

Now, you want to write a function taking a valid pair of a key and a value, `'x'` and `string`, `'y'` and `number`, `'z'` and `boolean`.

```
function x< Key extends keyof I >(key: Key, value: I[Key]): void {
    if (key === 'x') {
        // value: string | number | boolean
        // const s: string = value;
    }
}

x('x', 'foo');
x('y', 1);
x('z', true);
```

As I wrote in the comment, even in the `if` block, the type of `value` is `string | number | boolean`, so you cannot treat it as `string` without a cast.

This is because the function `x` can be instantiated with a union type. For example, when you pass a value of `'x' | 'y'` to `x`, type of `value` will be `string | number`.

```
const key: 'x' | 'y' = true ? 'x' : 'y' as const;
x(key, 1);
```

In this example, `x` will be instantiated as `function x(key: 'x' | 'y', value: string | number)`. So `value` can be a number even though you pass `'x'` as a key.

There is a trick to write this function in a type-safe manner.

```
type Pair<T> = { [K in keyof T]: [key: K, value: T[K]] }[keyof T];

function y(...[key, value]: Pair<I>): void {
    // Pair<T> = ['x', string] | ['y', number] | ['z', boolean]
    if (key === 'x') {
        const s: string = value;
    }
}

y('x', 'foo');
y('y', 1);
y('z', true);
```

As you can see, first, create a union of all pair types a key and a value can be using `Pair`. Then, define a function taking this pair type by deconstructing it.

Now, in the `if` block, a type of `value` is narrowed to `string`, and you no longer need to cast it.
