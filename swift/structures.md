# Structures
Everything that applies to classes in swift apply to structures, with the following caveats:

* Structures cannot inherit.
* Sturctures are always copied when passed.

Usde the struct keyword to decalre a sturcutre:
```swift
struct Point {
  var x: Int
  var y: Int
}
```
If no explicit initializer is provided for a struct, they use a default *memberwise initalizer*:
```swift
let p = Point(x: 2, y:3)
```
Structures are passed by value and thus copied, whereas classes are passed by reference. Thus reassiging to a class modifies the same point of memory, whereas structs remain persisten values.
