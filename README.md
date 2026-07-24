# Scala FP Demo - Library Management System

A portfolio project demonstrating **idiomatic Scala 3** and functional programming concepts through a library management system.

## What it demonstrates

| Concept | Where |
|---|---|
| Opaque types | `domain.scala` - `BookId`, `AuthorId` |
| Algebraic Data Types (ADTs) | `domain.scala` - `Genre` enum, `LibraryError` sealed hierarchy |
| Smart constructors | `domain.scala` - `ISBN.from`, `PublishYear.from` |
| Type classes (`Show`, `Eq`) | `typeclasses.scala` with `given` instances |
| Extension methods | `typeclasses.scala` - `.show`, `.===` |
| Immutable repository | `Library.scala` - pure functions, `Vector`/`Map` |
| `Either` error handling | `LibraryService.scala` - all operations typed |
| For-comprehensions | `LibraryService.scala` - `borrowBook`, `returnBook` |
| Higher-order functions | `LibraryService.scala` - `search`, `groupByGenre` |
| Pattern matching | Throughout - sealed traits, guards, destructuring |
| Unit tests | `LibraryServiceSuite.scala` - MUnit |

## Run

```bash
sbt run        # run the demo
sbt test       # run the test suite
```

## Stack

- **Scala 3.7.3**
- **sbt 1.11.6**
- **MUnit** (test framework)
