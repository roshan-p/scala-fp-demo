package com.roshanp

object Main extends App:

  def section(title: String): Unit =
    println(s"\n${"─" * 60}")
    println(s"  $title")
    println("─" * 60)

  def printResult[A: Show](label: String, result: Either[LibraryError, A]): Unit =
    result match
      case Right(value) => println(s"  ✓ $label: ${value.show}")
      case Left(err)    => println(s"  ✗ $label: ${err.message}")

  val authorMartin = Author(AuthorId("a1"), "Martin Odersky",    "Swiss",   Language.English)
  val authorAlvin  = Author(AuthorId("a2"), "Alvin Alexander",   "American",Language.English)
  val authorFowler = Author(AuthorId("a3"), "Martin Fowler",     "British", Language.English)
  val authorHarari = Author(AuthorId("a4"), "Yuval Noah Harari", "Israeli", Language.English)
  val authorLe     = Author(AuthorId("a5"), "Ursula K. Le Guin", "American",Language.English)

  def mkISBN(s: String) = ISBN.from(s).getOrElse(throw RuntimeException(s"bad isbn $s"))
  def mkYear(y: Int)    = PublishYear.from(y).getOrElse(throw RuntimeException(s"bad year $y"))

  val bookScala       = Book(BookId("b1"), "Programming in Scala",             AuthorId("a1"), mkISBN("9780981531687"), Genre.Programming, mkYear(2021), rating = Some(4.8))
  val bookCookbook    = Book(BookId("b2"), "Scala Cookbook",                   AuthorId("a2"), mkISBN("9781492051541"), Genre.Programming, mkYear(2021), rating = Some(4.5))
  val bookRefactor    = Book(BookId("b3"), "Refactoring",                      AuthorId("a3"), mkISBN("9780134757599"), Genre.Programming, mkYear(2018), rating = Some(4.7))
  val bookSapiens     = Book(BookId("b4"), "Sapiens: A Brief History",         AuthorId("a4"), mkISBN("9780062316097"), Genre.History,     mkYear(2011), rating = Some(4.6))
  val bookDisp        = Book(BookId("b5"), "The Dispossessed",                 AuthorId("a5"), mkISBN("9780061054884"), Genre.SciFi,       mkYear(1974), rating = Some(4.4))
  val bookLeftHand    = Book(BookId("b6"), "The Left Hand of Darkness",        AuthorId("a5"), mkISBN("9780441478125"), Genre.SciFi,       mkYear(1969), rating = None)

  val seedResult = for
    l1  <- Library.empty.addAuthor(authorMartin)
    l2  <- l1.addAuthor(authorAlvin)
    l3  <- l2.addAuthor(authorFowler)
    l4  <- l3.addAuthor(authorHarari)
    l5  <- l4.addAuthor(authorLe)
    l6  <- l5.addBook(bookScala)
    l7  <- l6.addBook(bookCookbook)
    l8  <- l7.addBook(bookRefactor)
    l9  <- l8.addBook(bookSapiens)
    l10 <- l9.addBook(bookDisp)
    l11 <- l10.addBook(bookLeftHand)
  yield l11

  val lib0 = seedResult.getOrElse(throw RuntimeException("seed failed"))

  println("╔══════════════════════════════════════════════════════════╗")
  println("║        Scala FP Demo — Library Management System        ║")
  println("║        Scala 3.7 · Opaque types · ADTs · Type classes   ║")
  println("╚══════════════════════════════════════════════════════════╝")

  // 1. Show type class
  section("1. TYPE CLASS: Show[A]")
  println(s"  Author : ${authorMartin.show}")
  println(s"  Book   : ${bookScala.show}")
  println(s"  Option : ${bookScala.rating.show}")
  println(s"  Either : ${(Right(bookScala.title): Either[String, String]).show}")

  // 2. Eq type class
  section("2. TYPE CLASS: Eq[A]")
  println(s"  bookScala === bookScala  → ${bookScala === bookScala}")
  println(s"  bookScala === bookCookbook → ${bookScala === bookCookbook}")

  // 3. Smart constructors
  section("3. SMART CONSTRUCTORS with Either validation")
  printResult("Valid ISBN",   ISBN.from("9780981531687"))
  printResult("Invalid ISBN", ISBN.from("not-an-isbn"))
  printResult("Valid year",   PublishYear.from(2024))
  printResult("Invalid year", PublishYear.from(1066))

  // 4. Pattern matching on ADT
  section("4. PATTERN MATCHING on sealed LibraryError")
  def describeError(err: LibraryError): String = err match
    case BookNotFound(id)        => s"Book not found: ${BookId.unapply(id)}"
    case AuthorNotFound(id)      => s"Author not found: ${AuthorId.unapply(id)}"
    case BookUnavailable(b)      => s"Book checked out: ${b.title}"
    case BookAlreadyAvailable(b) => s"Already on shelf: ${b.title}"
    case ValidationError(msg)    => s"Bad input: $msg"
    case DuplicateISBN(isbn)     => s"Duplicate ISBN: ${isbn.value}"

  List(BookNotFound(BookId("x99")), BookUnavailable(bookScala), DuplicateISBN(bookCookbook.isbn))
    .foreach(e => println(s"  ${describeError(e)}"))

  // 5. Borrow / Return (for-comprehension over Either)
  section("5. FOR-COMPREHENSION over Either — Borrow & Return")
  val (_, lib1) = LibraryService.borrowBook(BookId("b1"), lib0) match
    case Right((b, l)) => println(s"  ✓ Borrowed : ${b.show}"); (Right(b), l)
    case Left(err)     => println(s"  ✗ ${err.message}"); (Left(err), lib0)
  println()
  LibraryService.borrowBook(BookId("b1"), lib1) match
    case Right(_)  => println("  (unexpected success)")
    case Left(err) => println(s"  ✗ Expected failure: ${err.message}")
  println()
  val (_, lib2) = LibraryService.returnBook(BookId("b1"), lib1) match
    case Right((b, l)) => println(s"  ✓ Returned : ${b.show}"); (Right(b), l)
    case Left(err)     => println(s"  ✗ ${err.message}"); (Left(err), lib1)

  // 6. Higher-order search
  section("6. HIGHER-ORDER FUNCTIONS — Search")
  val progBooks = LibraryService.searchByGenre(Genre.Programming)(lib2)
  println(s"  Programming books (${progBooks.size}):")
  progBooks.foreach(b => println(s"    • ${b.show}"))
  println()
  val top3 = LibraryService.topRated(3)(lib2)
  println("  Top 3 rated:")
  top3.foreach(b => println(s"    • ${b.show}"))

  // 7. Group by genre
  section("7. groupBy — Books by genre")
  LibraryService.groupByGenre(lib2).foreach: (genre, books) =>
    println(s"  ${genre.show.capitalize} (${books.size}):")
    books.foreach(b => println(s"    • ${b.title} (${b.year.value})"))

  // 8. Recommendations
  section("8. RECOMMENDATION ENGINE")
  LibraryService.recommend(Genre.SciFi, lib2)
    .foreach(b => println(s"  • ${b.show}"))

  // 9. Bulk add with error accumulation
  section("9. BULK OPERATIONS — foldLeft accumulating errors")
  val dupBook = bookScala.copy(id = BookId("bX"))
  val newBook = Book(BookId("b7"), "Clean Code", AuthorId("a3"), mkISBN("9780132350884"), Genre.Programming, mkYear(2008), rating = Some(4.3))
  val (errs, lib3) = LibraryService.addBooks(Seq(dupBook, newBook), lib2)
  println(s"  Errors (${errs.size}): ${errs.map(_.message).mkString(", ")}")
  println(s"  Library size ${lib2.size} → ${lib3.size}")

  // 10. Statistics
  section("10. STATISTICS")
  val stats = LibraryService.statistics(lib3)
  println(s"  Total books   : ${stats.totalBooks}")
  println(s"  Available     : ${stats.availableBooks}")
  println(s"  Borrowed      : ${stats.borrowedBooks}")
  println(s"  Total authors : ${stats.totalAuthors}")
  println(s"  Avg rating    : ${stats.avgRating.show}")
  println("  Genre breakdown:")
  stats.genreBreakdown.toSeq.sortBy(-_._2).foreach: (g, n) =>
    println(s"    ${g.show.padTo(14, ' ')} ${"█" * n} ($n)")

  println(s"\n${"═" * 60}")
  println("  Demo complete. Run: sbt test")
  println("═" * 60)
