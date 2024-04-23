# `NSMapTable.count` counts released objects

Even though [the documentation](https://developer.apple.com/documentation/foundation/nsmaptable/1391360-count) isn't clear about this, but `NSMapTable.count` counts released objects.

```
  1> import Foundation
  2> let m = NSMapTable<NSObject, NSObject>.strongToWeakObjects()
m: NSMapTable<NSObject, NSObject> = <extracting data from value failed>

  3> m.setObject(NSObject(), forKey: NSNumber(1))
  4> let o = NSObject()
o: NSObject = {
  isa = NSObject
}
  5> m.setObject(o, forKey: NSNumber(2))
  6> m.count
$R0: Int = 2
  7> m.dictionaryRepresentation().count
  8.
$R1: Int = 1
```
