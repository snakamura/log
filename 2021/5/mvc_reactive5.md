# Classic MVC and Reactive Programming, part 5

This time, we're going to add a new complex business logic. We're going to calculate sum of all values!

In the classic MVC version, we'll add a new property named `sum` to the `Model`, and add a new `ModelListener` in the controller to update the view. Of course, you can update both views in one listener, but I wanted to demonstrate that a model can have multiple listeners.

```
class Model {
    ...

    get sum() {
        return this._values.reduce((sum, value) => sum + value, 0);
    }

    ...
}
```

```
class Controller {
    constructor(...) {
        ...

        model.addListener({
            updated(event: ModelEvent): void {
                $sum.innerText = event.model.sum.toString();
            }
        });

        ...
    }
}
```

The important thing is that it's a `Model` that runs this business logic. You shouldn't do this in the controller.

In the RxJS version, we'll update `values$` to have both values and sum of the values.  Its type becomes `Observable<[number[], number]>` instead of `Observable<number>`.

```
const values$ = fromEvent($add, 'click')
    .pipe(
        map(() => $value.value),
        filter(value => value.length > 0),
        map(value => Number(value)),
        filter(value => !isNaN(value)),
        scan((values, value) => [...values, value], [] as number[]),
        mergeWith(loadedValues$),
        map((values): [number[], number] => [values, values.reduce((sum, value) => sum + value, 0)]),
        share({
            connector: () => new BehaviorSubject([[], 0]),
        })
    );
```

Then, subscribe to it to update views.

```
values$.subscribe(([values, _]) => {
    $view.innerText = values.join(', ');
});

values$.subscribe(([_, sum]) => {
    $sum.innerText = sum.toString();
});
```

Just like it was important to run the business logic in the `Model`, it's important to make `values$` run the business logic instead of running it in each `Observer`.

You can find the complete code at [snakamura/mvc_rx](https://github.com/snakamura/mvc_rx/tree/master/6).

As you can see, `values$` itself behaves as a model. Then, what shall we do if we have more complex business logics? We'll look at it in the next post.
