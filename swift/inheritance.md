# Inheritance
Classes can inherit properties and methods from other classes.
Classes in swift can only have a single parent class. To inherit where Vehicle is the parent class:
```swift
class Car: Vehicle {
  ver engine = "V8"
}
```
Child classes can *override* the methods of their parents using the override keyword. You can call the parent version of the function within the override using the super keyword:
```swift
override func description() -> String {
  let description = super.description()
  return description + ", which is a car"
}
```


