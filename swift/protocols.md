# Protocols
Protocols are similar to interfaces in other languages, and define what methods and properties classes can declare. A class that fails to abide by a protocol results in a compilation error. Protocols are used heavily in swift.
```swift
protocol Blinkable {
  var isBlinking: Bool { get } //must be a gettable property
  var blinkSpeed: Double {get set} // must be gettable and settable
 
  func startBlinking(blinkSpeed: Double) -> Void // blinkable classes must have this function but can determine the implementation themselves.
}
```
Classes can conform to multiple protocols and contain methods and props not
determined by the protocol. At minimum they must implement all props and methods
defined in the protocol. Class protocols are defined like inheritance:
```swift
class TrafficLight: Blinkable {
  var isBlinking: Bool = false
  var blinkSpeed: Double = 0

  func startBlinking(blinkSpeed: Double) {
    print("Blinking now")
    isBlinking = true
    self.blinkSpeed = blinkSpeed
  }
}
```
You can then use protocols in functions or variable type constraints to abstract away the particulars of a class implementation and use the protocol as a type--i.e. a function can take any class as an argument so long as that class conforms to the protocol:
```swift
class Lighthouse: Blinkable {
  var isBlinking: Bool = false
  var blinkSpeed: Double = 0.0

  func startBlinking(blinkSpeed: Double) {
    print("A blinking lighthouse")
    isBlinking = true
    self.blinkSpeed = blinkSpeed
  }
}
var blinker: Blinkable
blinker = TrafficLight()
blinker = Lighthouse()
```
You can also assemble protocols together by requiring that a new protocol
conform to a previous protocol:
```swift
protocol ControlledBlink: Blinkable {
  func stopBlinking()
}
```
