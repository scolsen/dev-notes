# Asynchronous JS

Some approaches to asynchronous code:
* callbacks
* thunks
* promises
* generators/coroutines <- most important
* CSP (channel-oriented concurrency)

None of these is a "silver bullet" apply abstractions when needed.

## Parallel vs Async

Parallelism: Multiple threads on multiple cpu cores. Each core can run separate operations. OS virtual threads schedule threading across cores.
Asynchronous: Single thread. JS runs entirely within a single thread. Only one line of code runs at a given instance. JS cannot communicate across threads even when browsers implement things like webworkers that simulate multi-threading. Webworkers have to use async events to communicate.

## Concurrency
Two higher level tasks occur within the same time frame. (NOT at the same instance (parallel)). with JS we have to worry about *contention* processes on the single thread compete for resources. e.g. scheduling an ajax request before a page scroll blocks the scroll and renders a delay on the user. Concurrence enables us to interleave pieces of larger tasks on the single thread to give the illusion of simultaneous execution. This is a first come first serve model.

The scheduling is not always within the JS programmer's control.
But we **can** coordinate the order of responses.
Asynchronous programming is concurrency management. 

## Callbacks

Callbacks are problematic.
```javascript
setTimeout(function(){
  console.log("cb");  
}, 1000);
```
The above example splits the program into two parts "now" and "later"(things inside the callback)
The function is a "continuation" first half is executed now, the second is deferred. Callbacks== continuations.

Callback hell occurs when we must nest multiple callbacks.
The real issue with cb hell is not ugly indentation. using continuation passing style resolves this cosmetic issue. The more serious negative of callback hell lies in its cognitive dissonance. We do not reason in the same fashion as callbacks.


