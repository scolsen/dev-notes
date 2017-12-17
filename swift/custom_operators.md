# Custom Operators
You can define custom operators and custom implementations of existing operators for types a for instance, to add + functionality to a Vector class:
```swift
func +(left: Vector, right: Vector) -> Vector {
  let result = Vector(x: left.x + right.x, y: left.y + right.y)
  return result
}
```
To define new operators, use the infix, postfix, or prefix keywords:
```swift
infix operator $
```
*Note:* Remember, unicode characters, so symbols like ∀ can be defined as valid operators. 
