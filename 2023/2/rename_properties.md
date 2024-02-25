# Renaming properties in TypeScript

[Using `as` in a mapped type](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html#key-remapping-via-as), you can rename properties. For example, you can convert all properties to functions using this `Lazy` type.

```
type Lazy<T> = {
  [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K];
};
```

The `Person` interface below will be converted to an interface with three functions.

```
interface Person {
  readonly name: string;
  readonly age: number;
  readonly location: string;
}

type LazyPerson = Lazy<Person>;

// This is equivalent to
// interface LazyPerson {
//   readonly getName: () => string;
//   readonly getAge: () => string;
//   readonly getLocation: () => string;
// }
```

Then, is it possible to do the opposite conversion? Yes, you can do it with this `Strict` type.

```
type Strict<T extends { [key: `get${string}`]: () => unknown }> = {
  [K in keyof T as K extends `get${infer UnK}` ? Uncapitalize<UnK> : never]: K extends `get${string}` ? ReturnType<T[K]>: never;
};
```

As you can see, it picks properties whose name matches `getXxx` and converts its name to `xxx`, then git it a return type of the original function.

You can get the original `Person` from `LazyPerson` with it.

```
type StrictPerson = Strict<LazyPerson>;

// This is equivalent to
// interface StrictPerson {
//   readonly name: string;
//   readonly age: number;
//   readonly location: string;
// }
```

As I wrote above, this `Strict` omits all properties whose name doesn't match `getXxx`. So, for example, `X` won't have `y` in the following example.

```
type X = Strict<{
  getX: () => string;
  y: number;
}>;

// This is equivalent to
// interface X {
//   x: string;
// }
```

You can preserve other properties using `Strict2` below if you want.

```
type Strict2<T extends { [key: `get${string}`]: () => unknown }> = {
  [K in keyof T as K extends `get${infer UnK}` ? Uncapitalize<UnK> : K]: K extends `get${string}` ? ReturnType<T[K]>: T[K];
};
```

```
type X = Strict2<{
  getX: () => string;
  y: number;
}>;

// This is equivalent to
// interface X {
//   x: string;
//   y: number;
// }
```
