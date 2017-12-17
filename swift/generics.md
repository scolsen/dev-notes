# Generics 
Swift supports type generics for abstracting over types. Use <T> to define a generic (any term can be used between the brackets):
```swift
class Tree <T> {
  var value: T
  private(set) var childern: [Tree <T>] = [] //array of trees with same type as this class
  init(value: T) {
    self.value = value
  }
  addChild(value: T) -> Tree <T> {
    let newChild = Tree<T>(value: value)
    children.append(newChild)
    return newChild
  }
}
```
Then instantiate a generic as follows:
```swift
let intTree = Tree<Int>(value: 5)
intTree.addChild(value: 10)
```

