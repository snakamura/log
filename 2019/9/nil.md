# All nils are not equal in go

It’s interesting that this go code panics.

```
package main

import (
	"fmt"
)

type I interface {
    Name() string
}

type X struct {
    name string
}

func (x *X) Name() string {
    return x.name
}

func x() *X {
    return nil
}

func main() {
    var i I = x()
    if i != nil {
        fmt.Printf(i.Name())
    }
}
```

This is because `nil` returned from `x` has a type, and it's not `nil`. See [Why is my nil error value not equal to nil?](https://golang.org/doc/faq#nil_error) for details.
