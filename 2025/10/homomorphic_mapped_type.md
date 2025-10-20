# Homomorphic mapped types map `unknown` to `{}`

Imagine that you have this homomorphic mapped type in TypeScript.

```
type R<T> = { readonly [key in keyof T]: T[key] };
```

When you apply this to an object type or an array type, it maps them as expected.

```
// X1: { readonly x: string }
type X1 = R<{ x: string }>;

// X2: readonly string[]
type X2 = R<string[]>;
```

It also preserves primitive types.

```
// X3: string
type X3 = R<string>;

// X4: never
type X4 = R<never>;

// X5: undefined
type X5 = R<undefined>;
```

But it maps `unknown` to `{}`, and `any` to `{ readonly [x: string]: any }` , which I didn’t expect.

```
// X6: {}
type X6 = R<unknown>;

// X7: { readonly [x: string]: any }
type X7 = R<any>;
```
