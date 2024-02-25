# Smart cast doesn't work well with computed properties

In Kotlin, you can smart cast a property of a class in the same module, but you cannot do that when the class is in another module.

```
class X(val y: String?) {}

fun s(s: String) {}

val x = X()
if (x.y != null) {
    s(x.y) // This is valid only when X is in the same module
}
```

This is because the compiler cannot assume `y` isn't a computed property if `X` is in another module. It can return a different value when you call it twice if `y` is a computed property.

On the other hand, TypeScript seems to allow this.

```
interface I {
  readonly y: string | null;
}

function s(s: string) {
  console.log(s);
}

function i(i: I) {
  if (i.y != null) {
    s(i.y);
  }
}

class X implements I {
  get y(): string | null {
    return Math.random() > 0.5 ? 'y' : null;
  }
}

i(new X());
```

As you can see, this can print `null` even though `s` won't take `null`. In `i`, the compiler assumes `i.y` always returns the same value.
