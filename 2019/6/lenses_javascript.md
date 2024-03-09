# Lenses in Javascript

Imagine you have an array of strings and each string contains '\|'. You want to trim a prefix before '\|' and '\|' itself, and remove empty and duplicated strings. You might write code like this in Javascript.

```
const _ = require('lodash');

console.log('# 1')

const strings = ['a|1', 'a|2', 'b|1', 'c|'];

const convertedStrings1 = _.chain(strings)
    .map(s => s.substr(s.indexOf('|') + 1))
    .reject(_.isEmpty)
    .uniq()
    .value();
console.log(convertedStrings1);
```

This is pretty straightforward. Then, imagine you need to apply the same algorithm to an array of objects. You'll use `value` property of each object as a value. To do this, you can simply extend the previous code to work on an array of objects.

```
const objects = [
    {
        value: 'a|1',
        description: 'a-1'
    },
    {
        value: 'a|2',
        description: 'a-2'
    },
    {
        value: 'b|1',
        description: 'b-1'
    },
    {
        value: 'c|',
        description: 'c-'
    },
];

const convertedObjects1 = _.chain(objects)
    .map(o => ({
        value: o.value.substr(o.value.indexOf('|') + 1),
        description: o.description
    }))
    .reject(o => _.isEmpty(o.value))
    .uniqBy('value')
    .value();
console.log(convertedObjects1);
```

This is easy, but you'd want to reuse this algorithm for strings and objects. Having two codes for the same algorithm doesn't sound a good idea especially when the algorithm gets more complex, right?

The simple solution is to pass two functions to this algorithm. The first function gets a string from an element in an array. The second function updates an element with a specific string. Note that the second function returns a new string or object that has an updated value instead of mutating its parameter.

```
console.log('\n# 2');

function convert2(array, getValue, setValue) {
    return _.chain(array)
        .map(element => {
            const value = getValue(element);
            return setValue(element, value.substr(value.indexOf('|') + 1));
        })
        .reject(o => _.isEmpty(getValue(o)))
        .uniqBy(getValue)
        .value();
}
```

You can use this function to convert an array of strings and an array of objects.

```
const convertedStrings2 = convert2(strings, _.identity, (_, v) => v);
console.log(convertedStrings2);

const convertedObjects2 = convert2(objects,
    o => o.value,
    (o, value) => ({
        value,
        description: o.description
    }));
console.log(convertedObjects2);
```

This works pretty fine, but isn't it nice if we can pass one function instead of two? After all, both of these functions do something on a specific property of the value (or the value itself).

When it comes to combining a getter and a setter, lenses seem a good solution.

First, let's define `Functor` interface. Classes implementing this interface can encapsulate a value passed to their constructor, and apply a function to their internal value (`map` method).

```
console.log('\n# 3');

class Functor {
    constructor(v) {}
    map(f) {}
}
```

Then we'll define two classes implementing `Functor`; `Identity` and `Const`. These classes look similar, but while `map` of `Identity` applies a function to its internal value, `map` of `Const` ignores it.

```
class Identity extends Functor {
    constructor(v) {
        super(v);
        this._v = v;
    }

    get identity() {
        return this._v;
    }

    map(f) {
        return new Identity(f(this._v));
    }
}

class Const extends Functor {
    constructor(v) {
        super(v);
        this._v = v;
    }

    get const() {
        return this._v;
    }

    map(f) {
        return new Const(this._v);
    }
}
```

Using these classes, you can write a function to convert an array like this.

```
function convert3(array, lens) {
    const getValue = element => lens(v => new Const(v))(element).const;
    const setValue = (element, s) => lens(_.constant(new Identity(s)))(element).identity;
    return convert2(array, getValue, setValue);
}
```

Once you write "lenses" for strings and objects, you can pass them to this function to get the result.

```
const stringLens = convertIntoFunctor => string => convertIntoFunctor(string);
const objectLens = convertIntoFunctor => object => convertIntoFunctor(object.value).map(value => ({
    value,
    description: object.description,
}));

const convertedStrings3 = convert3(strings, stringLens);
console.log(convertedStrings3);

const convertedObjects3 = convert3(objects, objectLens);
console.log(convertedObjects3);
```

As you can see when you look at `objectLens`, a parameter to `convertIntoFunctor` defines `getValue` and a function passed to `map` defines `setValue`. So now we encapsulated two functions into one function in a clean way and pick them using `Identity` or `Const` functor.

These functions (`stringLens` and `objectLens`) are called "lenses" because they let you focus on a specific part of a value. For instance, `stringLens` lets you focus on the string value itself, and `objectLens` lets you focus on `value` property of the object value.

Furthermore, you can compose these lenses using function composition. Each lens is defined as a function taking a function and returns another function, so you can compose them using usual function composition. For instance, you can easily focus on `foo.bar.baz` by composing these lenses.

With `lens` function that creates a lens focusing on a specified property, you can apply `convert4` to objects with nested properties.

```
console.log("\n# 4")

const lens = name => convertIntoFunctor => object => convertIntoFunctor(object[name]).map(value => ({
    ...object,
    [name]: value,
}));

console.log(convert3([
    {
        foo: {
            bar: {
                baz: 'x|1'
            }
        },
        description: 'x-1'
    }
], _.flowRight(lens('foo'), lens('bar'), lens('baz'))));
```

In Javascript, using functors and lenses may be overdoing though.
