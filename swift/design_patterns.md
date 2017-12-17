# Design Patterns
The Cocoa framework supports three key patterns:
* model-view-controller(mvc)
* delegation
* notifications

## MVC
Model: objects that contain data or coordinate the storage, mgmt, and delivery of data to other objects. Only care about storing, do not care what happens to data they deliver.

View: Objects that face the user, providing information and receiving input. Only show data, do not manage it. Inform other objects when the user interacts with them.

Controller: Objects that mediate between models and views and contain most business logic. At minimum, receive info from models and pass to view, often doing something to the data before passing it along. Also passes info from views to models.

## Delegation
Delegation controls the passing of responsibilities between objects. Objects are stored as delegates on parent objects, which will send a signal to the delegates to inform them that something has occurred and that they must compute some operation. The parent object checks to see if the delegate implements a method that can handle the event, and if so, calls the method. An object can eb a delegate of multiple objects. Delegate messages are usually defined in protocols such as AVAudioPLayerDelegate, for example:
```swift
protocol HouseSecurityDelegate {
  func handleIntruder()  
}

class House {
  var delegate: HouseSecurityDelegate?

  func burglarDetected() {
    delegate?.handleIntruder() 
  }
}

class GaurdDog: HouseSecurityDelegate {
  func handleIntruder() {
    print("Releasing the hounds!")  
  }  
}

let house = House()
myHouse.burgalarDetected // does nothing

let dog = GaurdDog()
myHouse.delegate = dog
house.burglarDetected //prints Releasing the hounds!
```
The house can thus flexibly use any delegate object that conforms to the security protocol.
