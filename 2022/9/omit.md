# Omit in TypeScript isn't distributive

Imagine you have this type `X`.

```
type X = {
  readonly x: string;
} & (
  | {
    readonly type: 'Y';
    readonly y: number;
  }
  | {
    readonly type: 'Z';
    readonly z: boolean;
  }
);
```

And you want to define a type similar to `X` but without property `x`. You might think you can get it like this.

```
type X2 = Omit<X, 'x'>;
```

But when you try to use it, you'll find that the complier complains.

```
const o: X2 = {
  type: 'Y',
  y: 1,
};
```

Why? When you expand `X2`, you'll find it's like this. As you can see, it doesn't have neither `y` nor `z`.

```
type X2 = {
  readonly type: 'Y' | 'Z';
};
```

This is because `Omit` isn't distributive, and only gets the common properties of all the union types.

As you may have known, `keyof X` isn't `'x' | 'type' | 'y' | 'z'`, but `'x' | 'type'`. `Omit<X, 'x'>` has `Exclude<keyof X, 'x'>` as keys. So it'll only have `type` property.

You can define a distributive version of `Omit` like this.

```
type DistributiveOmit<T, K extends PropertyKey> = T extends unknown ? Omit<T, K> : never;
```

`DistributiveOmit<X, 'x'>` will be expanded to a union of each type without `x`.

```
DistributedOmit<X, 'x'> =
  | Omit<{
      readonly x: string;
    } & {
      readonly type: 'Y';
      readonly y: number;
    }>
  | Omit<{
      readonly x: string;
    } & {
      readonly type: 'Z';
      readonly z: boolean;
    }>;
```

So this definition is valid.

```
const o: DistributiveOmit<X, 'x'> = {
  type: 'Y',
  y: 1,
};
```

Note that you can have a distributive version of `keyof` this way.

```
type DistributiveKeyOf<t> = T extends unknown ? keyof T : never;
```

then, `DistributiveKeyOf<X>` will be expanded to `'x' | 'type' | 'y' | 'z'`.
