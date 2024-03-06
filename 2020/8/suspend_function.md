# Converting a function taking a callback to a suspend function

Imagine you have a function taking a callback function which will invoke the callback asynchronously. It can be a function invoking a callback after some dlay, or a function presenting an alert dialog and invoking a callback when a user closes the dialog.

First, you need import some packages to run the example below.

```
import java.util.Timer
import kotlin.concurrent.schedule
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine
import kotlinx.coroutines.runBlocking

```

Here is an example of this kind of function. This function invokes a callback after 1s.

```
fun callbackWithDelay(callback: (String) -> Unit) {
    val timer = Timer(false)
    timer.schedule(1000) {
        callback("done")
        timer.cancel()
    }
}
```

It's simple to call this function by passing a callback function, but it'd look nicer if you can call is as a suspend function. We need to convert this function to a suspend function to do that.

You can use [`suspendCoroutine`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.coroutines/suspend-coroutine.html) for this purpose. This function takes a function that takes a continuation and suspends until the continuation is resumed. Using it, you can have this suspend function.

```
suspend fun suspendWithDelay(): String = suspendCoroutine { cont ->
    callbackWithDelay { message ->
        cont.resume(message)
    }
}
```

Now, you can call this suspend function like this.

```
object Main {
    @JvmStatic
    fun main(args: Array<String>) = runBlocking<Unit> {
        val message = suspendWithDelay()
        println(message)
    }
}
```
