# Playing with wasm

## Installing tools

```
# Compiler
brew install emscripten

# Runtime
brew install wasmer

# Tool
brew install wabt
```

## Hello world

### via JavaScript

Prepare a source code named `helloworld.c`.

```
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Hello world\n");
    return 0;
}
```

Compile it with `emcc`.

```
emcc -o helloworld.js helloworld.c
```

This generates `helloworld.js` and `helloworld.wasm`. You can run it with Node.js.

```
node helloworld.js
```

In this case, the system interface will be given by JavaScript through `helloworld.js`, and `helloworld.js` calls `__main_argc_argv` exported from `helloworld.wasm` to start running the code.

When you run `wasm2wbt helloworld.wasm`, you can find these imports and exports.

```
(import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 13)))
(import "env" "emscripten_memcpy_js" (func (;1;) (type 7)))
```

```
(export "memory" (memory 0))
(export "__wasm_call_ctors" (func 2))
(export "__main_argc_argv" (func 3))
(export "__errno_location" (func 19))
(export "fflush" (func 52))
(export "__indirect_function_table" (table 0))
(export "emscripten_stack_init" (func 48))
(export "emscripten_stack_get_free" (func 49))
(export "emscripten_stack_get_base" (func 50))
(export "emscripten_stack_get_end" (func 51))
(export "stackSave" (func 53))
(export "stackRestore" (func 54))
(export "stackAlloc" (func 55))
(export "emscripten_stack_get_current" (func 56))
(export "dynCall_jiji" (func 58))
```

### Standalone

One the other hand, you can also compile it to a standalone wasm.

```
emcc -o helloworld.wasm helloworld.c
```

You can run it with wasmer.

```
wasmer helloworld.wasm
```

In this case, the system interface will be given by wasmer based on [WASI](https://wasi.dev/). wasmer calls `_start` exported from `helloworld.wasm` to start running the code.

You can see these imports and exports with `wasm2wbt`.

```
(import "wasi_snapshot_preview1" "args_sizes_get" (func (;0;) (type 5)))
(import "wasi_snapshot_preview1" "args_get" (func (;1;) (type 5)))
(import "wasi_snapshot_preview1" "proc_exit" (func (;2;) (type 3)))
(import "wasi_snapshot_preview1" "fd_write" (func (;3;) (type 11)))
```

```
(export "memory" (memory 0))
(export "_start" (func 6))
(export "__indirect_function_table" (table 0))
(export "__errno_location" (func 19))
(export "emscripten_stack_init" (func 54))
(export "emscripten_stack_get_free" (func 55))
(export "emscripten_stack_get_base" (func 56))
(export "emscripten_stack_get_end" (func 57))
(export "stackSave" (func 58))
(export "stackRestore" (func 59))
(export "stackAlloc" (func 60))
(export "emscripten_stack_get_current" (func 61))
```
