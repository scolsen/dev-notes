# Observers
Use observers to run code when a given property is modified. Observers can run before or after a value change.
Use the willSet and didSet blocks to set observers for a property:
```swift
class Observable {
  var number: Int = 0 {
    willSet(newNumber) {
      print("about to change to \(newNumber)")
    }
    didSet(oldNumber) {
      print("Number used to be \(oldNumber) but is now \(self.number)")
    }
  }
}
```
The willSet and didSet blocks have access to the incoming new value.
