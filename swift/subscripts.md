# Subscripts
Accessing members of arrays and dictionaries using [] is called subscripting, and can be implemented by your own classes. Use the subscript keyword and define how values are set and got using the subscript notation:
```swift
extension UInt8 {
  subscript(bit: UInt8) -> UInt8 {
    get {
      return (self >> bit & 0x07)
    }
    // run on val[x] = y
    set {
      let cleanBit = bit & 0x07
      let mask: UInt8 = 0xFF ^ (1 << cleanBit)
      let shiftedBit = (newValue & 1) << cleanBit
      self = self & mask | shiftedBit
    }
  }
}
```

