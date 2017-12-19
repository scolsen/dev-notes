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
The real issue with cb hell is not ugly indentation. Using continuation passing style resolves this cosmetic issue. The more serious negative of callback hell lies in its cognitive dissonance. We do not reason in the same fashion as callbacks.

Independent callbacks have to share closured state. (A shared variable). Managed time-state complexity, inelegant solution. Maintain some object to order things correctly. 

callback hell creates inversion of control. Parts of the program are split into code executed now (in control) and in callbacks (later control passed to 'someone else') i.e. we are not in control of how or when lines within a callback are executed.

If using other libraries passing in a callback is passing trust to the maintainers--e.g. credit charge, the owners of the library may call your code an unknown number of times in their function.

You have to trust the utility won't call it too many times, won't call to early, too late, too few times, won't swallow errors, etc. etc.

Callbacks are also not reasonable--difficult to be reasoned about. 

The brain typically works sequentially, iteratively, and adapts.

Callbacks subvert sequential ordering. 

Can be interrupted by events and respond, then continue sequencing. (JS engine/brain)

Callbacks are bad as the only structure for expressing temporal dependencies (sequencing with unknown Times)

The brain does not linearly progress through the code when overusing callbacks--this is the same reason people hate goto--requires jumping around that's cognitive dissonance.

callbacks cannot express asynchronity in a synchronous fashion. (promises etc. resolve this issue)

### Callback non-fixes

split callbacks: separate callback for errors. To avoid losing errors. Doubles trust issues.
node-style: error first style, first param is reserved for errors. Requires if else statements in every cb. Still falls prey to trust issues/ multicalls.

## Thunks

In synchronous programming a thunk is a function that provides all it requires to return some value--requires no arguments.

A thunk is a function with closured state that can track a value on each call.

A container around a collection of state.

An async func does not require any arguments other than a callback, used to get a value back.

```javascript
function addAsync(x, y, cb) {
  setTimeout(function(){
      cb(x + y); //ultimately passed to the thunk.
    }, 1000) 
}

var thunk = function(cb) {
  addAsync(10, 15, cb); 
}

thunk(function(sum){
    return sum;  //just returns whatever it is passed.
});
```

The thunk wrapper is time independent. It is always used in the same way. Time is the most complex factor of state management. 

Are a conceptual base for promise. Time independent wrappers around values.

It is possible to make thunk constructors.

Thunk ids a function hardcoded to call another function with set arguments and to return the result. 

However thunks do not solve inversion of control or all temporal dependency problems.

Lazy thunk doesn't do the work until called an initial time. We can have active thunks which do work immediately.

Lazy thunks do not request in parallel. They wait until completion, thus being synchronous.

Active thunks must call the async function immediately. Must return a single function accepting a callback as its only argument.

You can use closures to save the value of the captured async and pass them to the thunk's callback later on.

Alternatively use a closure to capture the callback, and call the callback within the async function.

You have to bridge closures, if checking each:
```javascript
function getFile(file) {
  var text, fn;
// this will work no matter which portion of this function runs first.
  fakeAjax(file, function(response){
    if (fn) fn(response);
    else text = response;
  });

  return function(cb){
    if(text) cb(text);
    else fn = cb;
  }
}
```
Thunk uses closure to maintain state.

Using closures to maintain state eliminates time concerns.
Thunks are composable. 
The thunking closure technique eliminates a race condition without use of a global state.

Promises capture this pattern.

## Promises



