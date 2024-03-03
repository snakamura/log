# Classic MVC and Reactive Programming, part 4

In [the previous post](https://snak.tumblr.com/post/651775445418770432/classic-mvc-and-reactive-programming-part-3), we added a support to load values, but didn't handle errors at all. It stopped working when you input an invalid values to the prompt.

In this post, we'll look at how we'll handle errors.

Before moving on, let's define a helper function that parses a string and returns an array of numbers. This function throws an error when it cannot parse a string as an array of numbers.

```
function parseNumberArray(string: string): number[] {
    const json = JSON.parse(string);
    if (!(json instanceof Array)) {
        throw new Error('Not an array.');
    }
    if (!json.every(v => typeof v === 'number')) {
        throw new Error('Not a number array.');
    }
    return json;
}
```

In the classic MVC version, you'll parse a string in `Model.load`, and catch an error in the controller and display it to a user. It's a model that knows what values and format can be accepted.

```
class Model {
    ...

    load(values: string) {
        this._values = parseNumberArray(values);
        this.fireUpdated();
    }

    ...
}
```

```
class Controller {
    constructor(...) {
        ...

        $load.addEventListener('click', () => {
            const values = window.prompt('Load');
            if (values != null) {
                try {
                    model.load(values);
                }
                catch (e) {
                    window.alert(`Error: ${ e.message }`);
                }
                $value.value = '';
            }
        });

        ...
    }
}
```

In the RxJS version, we can do similar things. We can directly catch an error in `map` operator. But in this post, I'll make it throw an error and catch it using `catchError`.

```
const loadedValues$ = fromEvent($load, 'click')
    .pipe(
        map(() => {
            const values = window.prompt('Load');
            return values != null ? parseNumberArray(values) : null;
        }),
        catchError((e, caught) => {
            window.alert(`Error: ${ e.message }`);
            return caught;
        }),
        filter((values: number[] | null): values is number[] => values != null)
    );
```

In the `map`, we'll return null if a user inputted nothing, and parse an input otherwise. When it fails to parse an input, `parseNumberArray` throws an error, and we can catch it using `catchError`. It should look natural to display this error, but why do we need to return `caught`?

When you use `return of(null)` instead of `return caught`, you'll find that it works fine until you notice that clicking the load button no longer works after that. This is because an `Observable` terminates once it emits an error. By returning `caught`, `catchError` restarts this `Observable`.

Another solution is to run an internal `Observable`. The outer main `Observable` won't terminate even when an internal `Observable` emits an error.

```
const loadedValues$ = fromEvent($load, 'click')
    .pipe(
        switchMap(() => {
            return of(window.prompt('Load'))
                .pipe(
                    filter((value: string | null): value is string => value != null),
                    map(value => parseNumberArray(value)),
                    catchError(e => {
                        window.alert(`Error: ${ e.message }`);
                        return of(null);
                    }),
                    filter((values: number[] | null): values is number[] => values != null),
                );
        }),
    );
```

In either case, you need to make sure that the `Observable` doesn't terminate even after an error occurred.

You can find the complete code at [snakamura/mvc_rx](https://href.li/?https://github.com/snakamura/mvc_rx/tree/master/4).

We'll try adding some business logics in the next post.
