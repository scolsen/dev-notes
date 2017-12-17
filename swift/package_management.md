# Swift Package Management
Swift provides the swift package manager for managing third party libraries and packages.
Swift manages packages by using packages files listing the dependencies of the project and building the listed packages into a library usable in the project.

To initialize a new swift package:
` swift package init --type executable `
This defines a new program as a package containing an executable as opposed to a library.
Swift scaffolds a project structure for us.
Store source code in the Sources folder.
Package.swift is our package manifest file. *Package.swift* must be stored int he root directory of the project.
Sample Package.swift:
```swift
import PackageDescription

let package = Package(
  name: "Dealer"
  dependencies: [
    .package(url: "test.com" from: "1.0.0"),
    .package(url: "http://github.com/something.git", from: "3.0.0")
  ],
  targets: [ 
    .target(
      name: "Dealer",
      dependencies: ["DeckOfPlayingCards"])
  ]
)
```
You must first import the PackageDescription library to define a package.
Targets define modules or test suites or other building blocks of a package and can depend on other targets in this package or in this package's dependencies.
Use ` swift build` to build your package targets. Use `swift run <targetName>` to run a target.
All built executables are stored in .build .debug stores debug builds and .release stores builds marked for release.
to generate an xcode project for a package:
`swift package generate-xcodeproj`
