# Mapping a function over a tuple, part 1

The other day, I was wondering if it was possible to map a function over a tuple in TypeScript while preserving each type in it. For example, imagine you have this tuple.

```
const objects = [
  { name: 'a' },
  { name: 'b' },
  { name: 'c' },
] as const;
```

I wanted to get `['a', 'b', 'c']` from it by mapping a function to get `name` from each object. Unfortunately, you'll get an array type `('a' | 'b' | 'c')[]` when you call `map` on it.

```
const name = <T>(v: { readonly name: T }): T => v.name;

// names: ('a' | 'b' | 'c')[]
const names = objects.map(name);
```

To preserve its tuple structure, you need to write your own function.

```
function tupleMap<T extends readonly unknown[], R>(
  t: T,
  f: (value: T[number], index: number) => R
): { readonly [key in keyof T]: R } {
  return t.map(f) as any;
}
```

When you apply `names` to it, you'll get a tuple.

```
// tupleNames: readonly ['a' | 'b' | 'c', 'a' | 'b' | 'c', 'a' | 'b' | 'c']
const tupleNames = tupleMap(objects, name);
```

But this doesn't give me `['a', 'b', 'c']`. This is because a type of `name` function will be unified to `(v: { readonly name: 'a' | 'b' | 'c' }): 'a' | 'b' | 'c'` when you pass it to `tupleNames` even though we want to pass it as a polymorphic function.

It's an option to pass a tuple of functions instead of passing a polymorphic function.

```
function tupleMap2<
  T extends readonly Record<PropertyKey, unknown>[],
  const F extends { readonly [index in keyof T]: (o: T[index]) => unknown },
>(
  t: T,
  f: F
): { readonly [index in keyof T]: F[index] extends (t: T[index]) => infer R ? R : never } {
  return t.map((o, index) => f[index](o)) as any;
}

// tupleNames2: readonly ['a', 'b', 'c']
const tupleNames2 = tupleMap2(
  objects,
  [
    v => v.name,
    v => v.name,
    v => v.name,
  ]
);
```

As you can see, we need to create a const tuple of functions because we get a return type of `toupleMap2` from a return type of each of these functions. Can we generate this tuple from `name` function? It's possible if you get a return type of each function from the tuple of objects. You need to pass a key to this function which property of an object it'll pick a type from.

```
function generateFunctions<K extends PropertyKey>(): <
  T extends readonly Record<K, unknown>[],
  F extends (o: Record<K, unknown>) => unknown,
>(
  t: T,
  f: F
) => { readonly [index in keyof T]: (o: { readonly [key in K]: T[index][K] }) => T[index][K] } {
  return (t, f) => t.map(() => f) as any;
}
```

`generateFunctions<'name'>()(objects, name)` returns `readonly [(o: { readonly name: 'a' }) => 'a', (o: { readonly name: 'b' }) => 'b', (o: { readonly name: 'c' }) => 'c']`. This means that you can pass it to `tupleMap2`.

```
// tupleNames3: readonly ['a', 'b', 'c']
const tupleNames3 = tupleMap2(
  objects,
  generateFunctions<'name'>()(objects, name)
);
```

We did it, but these implementations are full of casts to `any` everywhere, and they're not that type-safe. The main issue is that TypeScript doesn't support rank-2 types. We need to specify each function types separately, and we need to cast a polymorphic function to one of these function types manually.

What will it look like when we do the same thing in a language supporting rank-2 types such as Haskell?
