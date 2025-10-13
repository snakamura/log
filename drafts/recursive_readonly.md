# Making all properties readonly recursively

Imagine that you have this type `X`.

```
type X = {
  x: string;
  y: number;
};
```

You can make all properties `readonly` using `Readonly`.

```
// type ReadonlyX = {
//   readonly x: string;
//   readonly y: number;
// };
type ReadonlyX = Readonly<X>;
```

Then, what about this type `Y`?

```
type Y = {
  x: string;
  y: {
      p: string;
      q: number;
  };
};
```

`Readonly<Y>` makes `x` and `y` `readonly`, but doesn't make `p` and `q` `readonly`.

```
// type ReadonlyY = {
//   readonly x: string;
//   readonly y: {
//     p: string;
//     q: number;
//   };
// };
type ReadonlyY = Readonly<Y>;
```

You can write a recursive version of `Readonly` like this.

```
type RecursiveReadonly<T> = { readonly [key in keyof T]: RecursiveReadonly<T[key]> };
```

`p` and `q` become `readonly` with `RecursiveReadonly`.

```
// type RecursiveReadonlyY = {
//   readonly x: string;
//   readonly y: {
//     readonly p: string;
//     readonly q: number;
//   };
// };
type RecursiveReadonlyY = RecursiveReadonly<Y>;
```

But `RecursiveReadonly` doesn't work with `unknown` and `any` because [homomorphic mapped types map `unknown` to `{}` and `any` to `{ readonly [x: string]: any }`](./homomorphic_mapped_type.html). When you apply `RecursiveReadonly` to this `Z`,

```
type Z = {
  x: string;
  y: unknown;
  z: {
    p: string;
    q: any;
  };
};
```

`y` and `q` no longer are `unknown` and `any`.

```
// type RecursiveReadonlyZ = {
//   readonly x: string;
//   readonly y: {};
//   readonly z: {
//     readonly p: string;
//     readonly q: { readonly [x: string]: any };
//   };
// };
type RecursiveReadonlyZ = RecursiveReadonly<Z>;
```

This means that we need to handle `unknown` and `any` specially. Checking `T` extends `unknown` or `any` like this doesn't work because all types extend `unknown`.

```
type RecursiveReadonly<T> = T extends unknown
  ? T
  : { readonly [key in keyof T]: RecursiveReadonly<T[key]> };
```

Instead, you can check if `unknown` extends `T` because it holds only when `T` is `unknown`.

```
type RecursiveReadonly<T> = unknown extends T
  ? T
  : { readonly [key in keyof T]: RecursiveReadonly<T[key]> };
```

What about `any`? You can check if `T` is `any` by checking if `0` extends `T & 0`. This holds only when `T` is `any` (or `unknown`).

```
type RecursiveReadonly<T> = 0 extends (T & 0)
  ? T
  : { readonly [key in keyof T]: RecursiveReadonly<T[key]> };
```

To put them together, you can write `RecursiveReadonly` this way.

```
type RecursiveReadonly<T> = unknown extends T
  ? T
  : 0 extends (T & 0)
    ? T
    : { readonly [key in keyof T]: RecursiveReadonly<T[key]> };
```

As I wrote above, the second condition (`0 extends (T & 0)`) holds for `unknown` as well. So we can omit the first condition (`unknown extends T`) here, but you can still use it to distinguish `unknown` and `any`.

Of course, you can add some types such as `Function` and `RegExp` to the conditions if you don't want to make function properties and regexp properties, for example.
