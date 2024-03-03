# Classic MVC and Reactive Programming, part 3

Now that we can save values, let's try loading it. When we implemented saving values, we serialize values to a string and show it in an alert dialog. We'll load this string by showing a prompt dialog.

In the classic MVC version, we add `load` method to `Model`. This method replaces its values by new values, and fire an event so that the controller can update its views. The controller handles a click event on a button, shows a prompt and calls `load` with a string a user inputted.

```
class Model {
    ...

    load(values: string) {
        this._values = JSON.parse(values);
        this.fireUpdated();
    }

    ...
}

$load.addEventListener('click', () => {
    const values = window.prompt('Load');
    if (values != null) {
        model.load(values);
        $value.value = '';
    }
});
```

In the RxJS version, we'll have a new `Observable` that changes its value when a user clicks a button. A value of this `Observable` will be values you get by parsing a string a user inputted.

```
const loadedValues$ = fromEvent($load, 'click')
    .pipe(
        map(() => {
            const values = window.prompt('Load');
            if (values != null) {
                return JSON.parse(values);
            }
        })
    );
```

Then, we'll merge it into `values$`. Since `values$` holds values a user has inputted so far, we'll replace it by new values.

```
const values$ = fromEvent($add, 'click')
    .pipe(
        map(() => $value.value),
        filter(value => value.length > 0),
        map(value => Number(value)),
        filter(value => !isNaN(value)),
        scan((values, value, _) => [...values, value], [] as number[]),
        mergeWith(loadedValues$),
        share({
            connector: () => new BehaviorSubject<number>([]),
        })
    );
```

Notice that `values$` always has the latest values after adding a value and loading values. Observers subscribing to this `Observable` get the latest value without any changes.

You can find the complete code at [snakamura/mvc_rx](https://href.li/?https://github.com/snakamura/mvc_rx/tree/master/3).

In the next post, we'll see how to handle errors while loading values.
