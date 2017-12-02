# Extensions
You can extend existing types and add new methods and properties.
Extensions are preferred to inheritance as a method of adding new functionality to a class.
You can extend any type, including native types like String and Int.
Some types have extension restrictions.
To create an extension, use the extension keyword:
```swift
extension Int {
  var double: Int {
    return self * 2
  }
  func multiplyWith(anotherNumber: Int) -> Int {
    return self * anotherNumber
  }
}
```
extensions are available to every instance of a type.
*You can only add computed properties in extensions, not stored props.*
You can also use extensions to make types conform to protocols:
```swift
extension Int: Blinkable {
  //...implementation of Blinkable props and methods
}
```
You can also use an extension on a protocol to define a default implementation for the protocol:
```swift
extension Blinkable {
  func startBlinking(blinkSpeed: Double) {
    print("Blinking")
}
```
Now, any class that implements Blinkable and does not provide an implementation of startBlinking will use the default provided in the extension.

