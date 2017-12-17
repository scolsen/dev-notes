# Application Structure
macOs and iOS apps are built on *Event driven programming*. All application calls are a response to events of some form. Apps use a run loop at their core, and infinite loop that waits for events to fire and then handles them appropriately.

## Applications and Delegates
On iOS, the UIApplication class represents your application. However, this is managed by the OS and most of the time the programmer only needs to use the UIApplicationDelegate class.

UIView and UIWindow are view classes for our application. In iOS position(0,0) represents the top left corner of the screen.

