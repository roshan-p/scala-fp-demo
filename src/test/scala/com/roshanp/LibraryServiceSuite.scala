package com.roshanp

class LibraryServiceSuite extends munit.FunSuite:

  private val author = Author(AuthorId("a1"), "Martin Odersky", "Swiss")
  private def mkISBN(s: String) = ISBN.from(s).getOrElse(throw RuntimeException(s"bad isbn $s"))
  private def mkYear(y: Int)    = PublishYear.from(y).getOrElse(throw RuntimeException(s"bad year $y"))

  private def mkISBNFor(id: String): ISBN =
    val suffix = id.filter(_.isDigit).padTo(2, '0').takeRight(2)
    mkISBN(s"97800000000$suffix")

  private def makeBook(id: String, title: String, available: Boolean = true): Book =
    Book(BookId(id), title, AuthorId("a1"), mkISBNFor(id), Genre.Programming, mkYear(2020), available, Some(4.5))

  private val book1 = makeBook("b1", "Programming in Scala")
  private val book2 = makeBook("b2", "Scala Cookbook")

  private val lib0 =
    (for l1 <- Library.empty.addAuthor(author); l2 <- l1.addBook(book1); l3 <- l2.addBook(book2) yield l3)
      .getOrElse(throw RuntimeException("fixture"))

  test("ISBN.from rejects invalid ISBN"):
    assert(ISBN.from("bad").isLeft)
    assert(ISBN.from("9780981531687").isRight)

  test("PublishYear.from rejects pre-Gutenberg years"):
    assert(PublishYear.from(1000).isLeft)
    assert(PublishYear.from(2024).isRight)

  test("addBook returns DuplicateISBN for same ISBN"):
    lib0.addBook(book1.copy(id = BookId("bX"))) match
      case Left(DuplicateISBN(_)) => ()
      case other => fail(s"expected DuplicateISBN, got $other")

  test("findBook returns BookNotFound for missing id"):
    lib0.findBook(BookId("x999")) match
      case Left(BookNotFound(_)) => ()
      case other => fail(s"expected BookNotFound, got $other")

  test("borrowBook marks book unavailable"):
    LibraryService.borrowBook(BookId("b1"), lib0) match
      case Right((b, _)) => assert(!b.available)
      case Left(err) => fail(err.message)

  test("borrowBook fails when already borrowed"):
    val borrowed = makeBook("b3", "Borrowed Book", available = false)
    val lib = lib0.addBook(borrowed).getOrElse(throw RuntimeException("fixture"))
    LibraryService.borrowBook(BookId("b3"), lib) match
      case Left(BookUnavailable(_)) => ()
      case other => fail(s"expected BookUnavailable, got $other")

  test("returnBook marks book available"):
    val (_, libB) = LibraryService.borrowBook(BookId("b1"), lib0).getOrElse(throw RuntimeException("fixture"))
    LibraryService.returnBook(BookId("b1"), libB) match
      case Right((b, _)) => assert(b.available)
      case Left(err) => fail(err.message)

  test("returnBook fails when already available"):
    LibraryService.returnBook(BookId("b1"), lib0) match
      case Left(BookAlreadyAvailable(_)) => ()
      case other => fail(s"expected BookAlreadyAvailable, got $other")

  test("searchByTitle finds matches case-insensitively"):
    assertEquals(LibraryService.searchByTitle("scala")(lib0).size, 2)

  test("searchByTitle returns empty when no match"):
    assert(LibraryService.searchByTitle("haskell")(lib0).isEmpty)

  test("searchByGenre filters correctly"):
    assertEquals(LibraryService.searchByGenre(Genre.Programming)(lib0).size, 2)

  test("statistics totalBooks matches"):
    assertEquals(LibraryService.statistics(lib0).totalBooks, 2)

  test("statistics avgRating computed correctly"):
    LibraryService.statistics(lib0).avgRating match
      case Some(avg) => assertEqualsDouble(avg, 4.5, 0.001)
      case None => fail("expected avg rating")

  test("Show[Book] contains title"):
    assert(book1.show.contains(book1.title))

  test("Eq[Book] compares by id"):
    assert(book1 === book1)
    assert(book1 =/= book2)

  test("addBooks accumulates errors without stopping"):
    val dup  = book1.copy(id = BookId("bX"))
    val good = makeBook("b9", "Clean Code")
    val (errors, lib) = LibraryService.addBooks(Seq(dup, good), lib0)
    assertEquals(errors.size, 1)
    assertEquals(lib.size, 3)
