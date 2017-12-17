# Access Control
Swift has two points of access control operation, modules and source files. Swift has five levels of access control:

* Open and Public: Accessible by the current module and any module that imports the module.
* Internal: Only accessible in the module in which they are defined.
* Fileprivate: Only accessible in the source file in which the entity is declared.
* Private: Only accessible within the current declaration scope.
public classes cannot be subclassed in other modules, whereas open classes can be subclassed in other modules.

Methods cannot be less private than the classes that declare them. (A private class cannot contain a public method)
Properties and methods are internal by default.
to define the access control for an entity, use an access keyword:
```swift 
private class something {
  //definitions
}
```
properties and methods of private classes are private by default.
Make a property read-only by declaring its setter as private:
```swift
private(set) var privateprop = 123
```
You can use the final keyword to prevent a subclass from overriding a member or class:
```swift
final class FinalClass{
  //definitions
}

class notFinalButFinalMethod {
  final func something() {
    //stuff
  }
}

```

