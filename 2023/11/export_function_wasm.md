# Exporting a function from wasm

Let's write a small wasm module which only exports one function named `double`. This function doubles its argument.

You can write it with any language that can be compiled to wasm, but I wrote it in wbt (WebAssembly Text Format) directly.

```wbt:double.wat
(module
  (func $double (param $n i32) (result i32)
    local.get $n
    local.get $n
    i32.add
  )
  (export "double" (func $double))
)
```

This module exports `double` function which takes one `i32` as a parameter and returns `i32`. It pushes the parameter to the stack twice using `local.get`, and call `i32.add` which pops these values from the stack, adds them and pushes the result to the stack. This value on the stack will be returned to the caller of `double` directly (In WebAssembly, we return a value from a function by pushing it to the stack).

When you name this file `double.wbt` and run `wbt2wasm double.wbt`, you'll get `double.wasm`.

Now, let's call this function. This time, we'll call it from JavaScript.

```js:export.mjs
import { readFile } from 'fs/promises';

const wasm = await readFile('double.wasm');
const module = await WebAssembly.instantiate(wasm);
const double = module.instance.exports.double;
const result = double(5);
console.log(result);
```

When you instantiate this module using `WebAssembly.instantiate`, you can get a function via its `instance.exports` property.

Saving this as `export.mjs` and running `node export.mjs` will print `10`.
