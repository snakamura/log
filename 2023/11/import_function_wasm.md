# Importing a function to wasm

This time, we're going to see how you can import a function from somewhere in wasm.

```wbt:print.wat
(module
  (import "env" "log" (func $log (param i32)))
  (func $print (param $n i32)
    local.get $n
    call $log
  )
  (export "print" (func $print))
)
```

This module imports a function to log a number, and exports a function to print a number using it. To import a function, you can use `import` by specifying namespaces (`env` and `log`) and its types. This time, it takes `i32` and returns nothing.

You'll push an argument to the stack and use `call` to call this function.

When you name this file `print.wbt` and run `wbt2wasm print.wbt`, you'll get `print.wasm`.

Now, how can you pass this log function? This time, I'll invoke `print` from JavaScript by passing `console.log` as this imported function.

```js:import.mjs
import { readFile } from 'fs/promises';

const wasm = await readFile('print.wasm');
const module = await WebAssembly.instantiate(
  wasm,
  {
    env: {
      log: console.log,
    },
  }
);
const print = module.instance.exports.print;
print(13);
```

As you can see, you can pass an object containing a function to `WebAssembly.instantiate`. `print.wasm` can access it by importing them by its name. This time, it's `env` and `log`.

Saving this as `import.mjs` and running `node import.mjs` will print `13`.
