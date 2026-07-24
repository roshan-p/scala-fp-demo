package com.roshanp

/** Pure service layer — all operations return `Either[LibraryError, (A, Library)]`
  * where the new `Library` is the updated state after the operation.
  *
  * Demonstrates:
  *  - for-comprehensions over Either
  *  - Higher-order functions (search, recommend)
  *  - Pattern matching with guards
  *  - Immutable state threading
  */
object LibraryService:

  // ─── Borrow / Return ──────────────────────────────────────────────────────

  def borrowBook(
      bookId: BookId,
      library: Library
  ): Either[LibraryError, (Book, Library)] =
    for
      book        <- library.findBook(bookId)
      _           <- Either.cond(book.available, (), BookUnavailable(book))
      updatedBook  = book.copy(available = false)
      newLibrary  <- library.updateBook(updatedBook)
    yield (updatedBook, newLibrary)

  def returnBook(
      bookId: BookId,
      library: Library
  ): Either[LibraryError, (Book, Library)] =
    for
      book        <- library.findBook(bookId)
      _           <- Either.cond(!book.available, (), BookAlreadyAvailable(book))
      updatedBook  = book.copy(available = true)
      newLibrary  <- library.updateBook(updatedBook)
    yield (updatedBook, newLibrary)

  // ─── Search (higher-order function) ──────────────────────────────────────

  def search(
      library: Library,
      predicate: Book => Boolean
  ): Vector[Book] =
    library.books.values.filter(predicate).toVector.sorted(using Ordering.bookByTitle)

  def searchByTitle(keyword: String)(library: Library): Vector[Book] =
    search(library, _.title.toLowerCase.contains(keyword.toLowerCase))

  def searchByGenre(genre: Genre)(library: Library): Vector[Book] =
    search(library, _.genre == genre)

  def searchAvailable(library: Library): Vector[Book] =
    search(library, _.available)

  def searchHighRated(minRating: Double)(library: Library): Vector[Book] =
    search(library, _.rating.exists(_ >= minRating))

  // ─── Analytics ────────────────────────────────────────────────────────────

  def groupByGenre(library: Library): Map[Genre, Vector[Book]] =
    library.books.values
      .toVector
      .groupBy(_.genre)
      .map((genre, books) => genre -> books.sorted(using Ordering.bookByTitle))

  def topRated(n: Int)(library: Library): Vector[Book] =
    library.books.values
      .toVector
      .filter(_.rating.isDefined)
      .sorted(using Ordering.bookByRating)
      .take(n)

  def statistics(library: Library): LibraryStats =
    val books     = library.books.values.toVector
    val ratings   = books.flatMap(_.rating)
    LibraryStats(
      totalBooks     = books.size,
      availableBooks = books.count(_.available),
      borrowedBooks  = books.count(!_.available),
      totalAuthors   = library.authors.size,
      avgRating      = if ratings.isEmpty then None
                       else Some(ratings.sum / ratings.size),
      genreBreakdown = books.groupBy(_.genre).map((g, bs) => g -> bs.size)
    )

  // ─── Recommendation engine ────────────────────────────────────────────────

  /** Recommends books based on a favourite genre, sorted by rating then title. */
  def recommend(
      favouriteGenre: Genre,
      library: Library,
      limit: Int = 3
  ): Vector[Book] =
    val byGenre   = library.booksByGenre(favouriteGenre).filter(_.available)
    val withRating = byGenre.filter(_.rating.isDefined).sorted(using Ordering.bookByRating)
    val withoutRating = byGenre.filter(_.rating.isEmpty).sorted(using Ordering.bookByTitle)
    (withRating ++ withoutRating).take(limit)

  // ─── Bulk operations ──────────────────────────────────────────────────────

  /** Adds multiple books, collecting all errors instead of stopping at the first. */
  def addBooks(
      books: Seq[Book],
      library: Library
  ): (Vector[LibraryError], Library) =
    books.foldLeft((Vector.empty[LibraryError], library)):
      case ((errors, lib), book) =>
        lib.addBook(book) match
          case Right(updated) => (errors, updated)
          case Left(err)      => (errors :+ err, lib)

// ─── Stats model ──────────────────────────────────────────────────────────────

final case class LibraryStats(
    totalBooks: Int,
    availableBooks: Int,
    borrowedBooks: Int,
    totalAuthors: Int,
    avgRating: Option[Double],
    genreBreakdown: Map[Genre, Int]
)
