# Associated types in TypeScript

In Swift, you can associate two types by declaring `associatetype` then defining it using `typealias`.

```
protocol Type {
    var name: String { get }
}

struct TypeX: Type {
    let name = "X"
}

struct TypeY: Type {
    let name = "Y"
}

protocol Value {
    associatedtype ValueType
    var type: ValueType { get }
}

struct ValueX: Value {
    typealias ValueType = TypeX
    let type = TypeX()
}

struct ValueY: Value {
    typealias ValueType = TypeY
    let type = TypeY()
}

protocol User {
    associatedtype V: Value
    var type: V.ValueType { get }
    var value: V { get }
}

class UserX: User {
    typealias V = ValueX
    var type: TypeX { return TypeX() }
    var value: ValueX { return ValueX() }
}

let x = UserX()
print(x.type.name)
print(x.value.type.name)
```

How can we do this in TypeScript? In TypeScript, you cannot put an associated type in an interface. Instead, you need to pass it as a generic parameter to an interface, then use a conditional type to retrieve it.

```
type Type = 'X' | 'Y'

interface Value<Type> {
    type: Type
}

class ValueX implements Value<'X'> {
    type: 'X' = 'X'
}

class ValueY implements Value<'Y'> {
    type: 'Y' = 'Y'
}

type ValueType<V> = V extends Value<infer T> ? T : never

interface User<V extends Value<any>> {
    type: ValueType<V>;
    value: V;
}

class UserX implements User<ValueX> {
    type: 'X' = 'X';
    value = new ValueX();
}

const x = new UserX();
console.log(x.type, x.value.type);
```

Here, `ValueType` is a conditional type and it'll be inferred to a generic type passed to `Value` interface.
