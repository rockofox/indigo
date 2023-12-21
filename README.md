<img align="right" width="140" height="130" src="assets/logo.png" alt="indigo logo">
<h3 align="left">Indigo</h3>

Indigo is an **experimental** multiparadigm programming language designed to lay focus on functional programming, clearity and convinience.
Inspired by F# and Haskell, it aims to deliver satisfactory functional programming features, while exploring new concepts.
```julia
let greet (name: String) => IO = println "Hello " : name
let main => IO = do
    let person = Person { name: "Lily", age: 22 }
    greet person.name
end
```
```julia
map (`*`2), [2, 4, 6, 8]     # [4, 8, 12, 16]
```
### Features
* Structures
* Traits
* Polymorphism
* Partial Functions
* Lambda/Anonymous functions
* Pattern Matching
* Types
* Function Composition/Pipelines
* Imports

### Roadmap
* Documentation
* Generics
* Contracts/Constraints
* Uniform Function Call Syntax
* Named and optional parameters
