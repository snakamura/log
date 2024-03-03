# Classic MVC and Reactive Programming, part 6

In [the previous post](https://snak.tumblr.com/post/652207940931092480/classic-mvc-and-reactive-programming-part-5), I added a new business logic, and made `values$` hold a calculated value.

There're some problems about this. First, it calculates the value even when it's not used. Second, the tuple containing the value will get larger when you add more business logics.

So now, we'll reintroduce `Model` class. This class looks similar to the `Model` in the classic MVC version, but it's immutable. For instance, `addValue` returns a new `Model` instead of mutating it.

Since it's immutable, we don't need any listeners and events.

```
class Model {
    constructor(values: number[] = []) {
        this._values = values;
    }

    get values() {
        return [...this._values];
    }

    get sum() {
        return this._values.reduce((sum, value) => sum + value, 0);
    }

    addValue(value: number): Model {
        return new Model([...this._values, value]);
    }

    save(): string {
        return JSON.stringify(this._values);
    }

    static load(values: string): Model {
        return new Model(parseNumberArray(values));
    }

    private readonly _values: number[];
}
```

Now, we use this class instead of an array of numbers as an `Observable`.

```
class Controller {
    constructor(document: HTMLDocument) {
        const $value = document.getElementById('value') as HTMLInputElement;
        const $add = document.getElementById('add') as HTMLInputElement;
        const $save = document.getElementById('save') as HTMLInputElement;
        const $load = document.getElementById('load') as HTMLInputElement;
        const $view = document.getElementById('view') as HTMLInputElement;
        const $sum = document.getElementById('sum') as HTMLInputElement;

        const loadedModel$ = fromEvent($load, 'click')
            .pipe(
                map(() => {
                    const values = window.prompt('Load');
                    return values != null ? Model.load(values) : null;
                }),
                catchError((e, caught) => {
                    window.alert(`Error: ${ e.message }`);
                    return caught;
                }),
                filter((model: Model | null): model is Model => model != null)
            );

        const model$ = fromEvent($add, 'click')
            .pipe(
                map(() => $value.value),
                filter(value => value.length > 0),
                map(value => Number(value)),
                filter(value => !isNaN(value)),
                scan((model, value) => model.addValue(value), new Model()),
                mergeWith(loadedModel$),
                share({
                    connector: () => new BehaviorSubject<model>(new Model()),
                })
            );

        model$.subscribe(model => {
            $view.innerText = model.values.join(', ');
        });

        model$.subscribe(model => {
            $sum.innerText = model.sum.toString();
        });

        model$.subscribe(_ => {
            $value.value = '';
        });

        fromEvent($save, 'click')
            .subscribe(() => {
                model$.subscribe(model => {
                    window.alert(model.save());
                }).unsubscribe();
            });
    }
}
```

When you compare this with the classic MVC version, you'll find that we kind of split the `Model` into two parts. First, we converted a mutable `Model` to a immutable `Model`. Second, we converted listeners and events to an `Observable`.

In the classic MVC version, the `Model` itself represents a state, but now, we use `Observable<model>` to represent the state.

You can find the complete code at [snakamura/mvc_rx](https://href.li/?https://github.com/snakamura/mvc_rx/tree/master/7).
