# Error Handling
Unlike Objective-C, swift does not use NSError. In swift, errors are any type that conform to the Error protocol. Any class, enum, or struct can be an error. For example, the following is an enumeration error:
```swift
enum BankError: Error {
  case insufficientFunds
  case cannotBeginWithNegativeFunds
  case cannotMakeNegativeTransaction(amount: Float)
}
```
Functions that throw errors must be marked with the throw keyword, which must be included after the functions return type:
```swift
class BankAccount {
  private(set) var balance: Float = 0.0
  init(amount: Float) throws {
      gaurd amount > 0 else {
          throw BankError.cannotBeginWithNegativeFunds
        } 
        balance = amount
    }
  func deposit(amount: Float) throws {
    gaurd amount > 0 else {
      throw BankError.cannotMakeNegativeTransaction  
    }    
    balance += amount
  }
}
```
When you call a function that throws, you are required to wrap it in a do-catch block in order to handle the error, and you must preface each throwing function call with try:
```swift
do {
  let vacationFund = try BankAccount(amount: 5)
  try vactionFund.deposit(amount: 10)
} catch let error as BankError {
  switch(error) {
    case .notEnoughFunds:
      print("Error: not enough funds")
    case .cannotMakeNegativeTransaction(let amount):
      print("Error: cannot make transaction with negative funds \(amount)")
  } 
} catch let error {
  // optional block to catch other types of errors.  
}
```
IF the details of the error do not matter, you can use try? instead of a do-catch block. try? will return nil if an error is thrown:
```swift
let secretBankAccount = try? BankAccount(amount: -50) //returns nil, cannot make account w/ negsative amount 
```
If you need the program to crash on an error, use try!, which will crash the program on an error:
```swift
let secretBankAccount = try! BankAccount(amount 50) //this call will crash the program if we use an incorrect amount such as a negative
```

