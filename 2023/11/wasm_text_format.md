# WebAssembly text format

The text format of WebAssembly is described in [Text Format](https://webassembly.github.io/spec/core/text/index.html). It contains several objects such as a module, a function and so on.

In the basic format, you'll just list instructions in the body of a function. For example, this is a function that calculates sum of numbers up to a specified number.

```wbt:sum_up_to1.wat
(module
  (func $sumUpTo
    (param $n i32)
    (result i32)

    (local $sum i32)

    i32.const 0
    local.set $sum

    loop $loop
      local.get $n
      if
        local.get $sum
        local.get $n
        i32.add
        local.set $sum

        local.get $n
        i32.const 1
        i32.sub
        local.set $n

        br $loop
      end
    end

    local.get $sum
  )
  (export "sumUpTo" (func $sumUpTo))
)
```

You can find that it just lists instructions if you ignore indentations.

On the other hand, you can fold these instructions if you'd like. In this format, operands of an instruction follow the instruction.

```wbt:sum_up_to2.wat
(module
  (func $sumUpTo
    (param $n i32)
    (result i32)

    (local $sum i32)

    (local.set $sum
      (i32.const 0)
    )

    (loop $loop
      (if (local.get $n)
        (then
          (local.set $sum
            (i32.add
              (local.get $sum)
              (local.get $n)
            )
          )
          (local.set $n
            (i32.sub
              (local.get $n)
              (i32.const 1)
            )
          )
          br $loop
        )
      )
    )

    local.get $sum
  )
  (export "sumUpTo" (func $sumUpTo))
)
```
