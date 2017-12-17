# Memory Management
Swift uses *reference counting* to manage memory. When an object is assigned to a var, the *retian count* increases by one. When an object is no longer used, the retain count decreases. If the retain count for an object reaches 0 (it is no longer used by any variables) the object is removed from memory.

## Retain Cycles
Retain cycles occur when you have two or more objects that refer to each other, but are not referred to anywhere else in the application. Such objects refer to each other and thus do not have a retain of 0, but are also not referenced anywhere else, and are thus unusable but will never be released and are hogging memory. To avoid this issue, use a *weak reference* which is a variable that refers to an object, but does not modify the retain count of the object. Use weak references when you don't care whether or not an object remains in memory:
```swift
class Human {
  weak var bestfriend: Dog?
  var name: String
  init(name: String) {
    self.name = name   
  }
  deinit {
    print("\(name) is being removed")   
  }
}

class Dog {
  weak var bestfriend: Human?
  var name: String
  init(name: String){
    self.name = name
  }
  deinit {
    print("\(name) is being removed")  
  }
}
```
The above code uses weak references. If it did not, once any human or dog were assigned a bestfriend the assigned object is stuck in memory indefinitely and cannot be removed by nil. Thus use weak when some code has a reference to an object but should not 'own' it.
Weak references *must* be an optional. However, for the situation in which one object should be optional for another but should only exist as long as that object, as in the case of persons and passports, we should instead use the *unowned* reference:
```swift
class Person {
  var name: String
  var passport: Passport?
  init(name: String){
    self.name  = name  
  }
  deinit {
    print("\(name) is being removed")  
  }
}

class Passport {
  var number: Int
  unowned let person: Person
  init(number: Int, person: Person){
    self.number = number
    self.person = person
  }
  deinit{
    p[rint("passport \(number) is being removed" 
  }
}
```
If you deallocate an object before your code stopped using it your program will crash.
