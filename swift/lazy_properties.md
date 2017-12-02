# Lazy Properties
Lazy properties are not set until the first time they are accessed. Use the lazy keyword to define a property as lazy:
```swift
class SomeMemoryConsumer {
  init(id: Int) {
    print("created \(id)")
  }
}
class LazyMode {
  lazy var lazyClassLoad = SomeExpensiveClass(id: 2)

  init() {
    print("Lazy class created")
  }
}

var lazyExample = LazyMode()
lazyExample.lazyClassLoad // prints "created 2"
```
Lazy only applies to var and cannot be used on let.
