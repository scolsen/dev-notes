# Computed Properties
Computed properties are an convenient way of establishing getters and setters
for properties composed using other properties of an object. For example:
```swift
class Rect {
  var width: Double = 0.0
  var height: Double = 0.0

  var area: Double {
    get {
      return width * height
    }
    
    set {
      width = sqrt(newValue)
      height = sqrt(newValue)
    }
  }
}
```

The newValue constant is accessible to setters for defining new property values

When the computed property does not require a setter, we can use a shorter
definition:

```swift
class Rect {
  var centre: (x: Double, y: Double){
    return (width/2, height/2)
  }
}
```

To access a computed property:
```swift 
rect.centre //(x: 1.5, y: 1.5)
```

To set a computed property: 
```swift
let rect = Rect()
rect.area = 9
```
