package com.roshanp

/** Immutable in-memory library store.
  *
  * All operations return a new `Library` — no mutation anywhere.
  * This is a pure functional data structure.
  */
final case class Library(
    books: Map[BookId, Book]     = Map.empty,
    authors: Map[AuthorId, Author] = Map.empty
):
  // ─── Book operations ──────────────────────────────────────────────────────

  def addBook(book: Book): Either[LibraryError, Library] =
    books.values.find(b => Eq[ISBN].eqv(b.isbn, book.isbn)) match
      case Some(_) => Left(DuplicateISBN(book.isbn))
      case None    => Right(copy(books = books + (book.id -> book)))

  def removeBook(id: BookId): Either[LibraryError, Library] =
    books.get(id) match
      case None    => Left(BookNotFound(id))
      case Some(_) => Right(copy(books = books - id))

  def updateBook(updated: Book): Either[LibraryError, Library] =
    books.get(updated.id) match
      case None    => Left(BookNotFound(updated.id))
      case Some(_) => Right(copy(books = books + (updated.id -> updated)))

  def findBook(id: BookId): Either[LibraryError, Book] =
    books.get(id).toRight(BookNotFound(id))

  def findBookByISBN(isbn: ISBN): Option[Book] =
    books.values.find(b => Eq[ISBN].eqv(b.isbn, isbn))

  // ─── Author operations ────────────────────────────────────────────────────

  def addAuthor(author: Author): Either[LibraryError, Library] =
    Right(copy(authors = authors + (author.id -> author)))

  def findAuthor(id: AuthorId): Either[LibraryError, Author] =
    authors.get(id).toRight(AuthorNotFound(id))

  // ─── Query helpers ────────────────────────────────────────────────────────

  def availableBooks: Vector[Book] =
    books.values.filter(_.available).toVector

  def borrowedBooks: Vector[Book] =
    books.values.filterNot(_.available).toVector

  def booksByGenre(genre: Genre): Vector[Book] =
    books.values.filter(_.genre == genre).toVector

  def booksByAuthor(authorId: AuthorId): Vector[Book] =
    books.values.filter(b => Eq[AuthorId].eqv(b.authorId, authorId)).toVector

  def size: Int = books.size

object Library:
  val empty: Library = Library()
