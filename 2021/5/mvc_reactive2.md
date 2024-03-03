# Classic MVC and Reactive Programming, part 2

Next, let's support saving values that a user has inputted.

In the classic MVC version, you'll add a new `save` method to `Model` to serialize values to some form, and call it when a user clicks the save button. In this example, we'll serialize values to a string and show it using `alert` instead of saving values to a file or a server.

```
class Model {
    ...

    save(): string {
        return JSON.stringify(this._values);
    }

    ...
}
```

```
$save.addEventListener('click', () => {
    window.alert(model.save());
});
```

How do we implement this with RxJS? One way of implementing this is to store values in the controller, handle a click event on the save button, then save it.

```
values$.subscribe(values => {
    $view.innerText = values.join(', ');
    $value.value = '';

    // Store values here
    this.values = values;
});

fromEvent($save, 'click')
    .subscribe(() => {
        // Use stored values
        window.alert(JSON.stringify(this.values));
    });
```

In this scenario, you use `Observable`s as event emitters. I don't think this is a very good approach. Instead, let's think that a `Observable` is a value whose value changes.

In this approach, you can think that these two `Observable`s are equivalent after a user has inputted 1, 2 and 3.

```
fromEvent($add, 'click')
    .pipe(
        map(() => $value.value),
        filter(value => value.length > 0),
        map(value => Number(value)),
        filter(value => !isNaN(value))
    );
```

```
of(1, 2, 3);
```

If it was `of(1, 2, 3)`, you can get values by subscribing to it. Let's try the same thing with `values$`.

```
fromEvent($save, 'click')
    .subscribe(() => {
        values$.subscribe(values => window.alert(JSON.stringify(values))).unsubscribe();
    });
```

When a user clicks the save button, subscribe to `values$` to get the latest values and save the values.

Unfortunately, this doesn't work because an `Observable` created by `fromEvent` doesn't call `next` until a user clicks the add button next time. To make this work, you need to convert this `Observable` to `BehaviorSubject` which stores the latest value and emits it when it's subscribed.

You can explicitly create `BehaviorSubject` or use `share` operator.

```
const values$ = fromEvent($add, 'click')
    .pipe(
        map(() => $value.value),
        filter(value => value.length > 0),
        map(value => Number(value)),
        filter(value => !isNaN(value)),
        scan((values, value, _) => [...values, value], [] as number[]),
        share({
            connector: () => new BehaviorSubject<number>([]),
        })
    );
```

Now you can get the latest values by subscribing to `values$` when a user clicks the save button.

`values$` is no longer just an event emitter, but it's a value that changes over time, and you can get it by subscribing to it.

You can find the complete code at [snakamura/mvc_rx](https://href.li/?https://github.com/snakamura/mvc_rx/tree/master/2).

In the next post, I'll add support to load values.
