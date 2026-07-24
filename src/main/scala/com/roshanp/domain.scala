package com.roshanp

// ─── Opaque types — compile-time type safety with zero runtime overhead ────────

opaque type BookId   = String
opaque type AuthorId = String

object BookId:
  def apply(value: String): BookId   = value
  def unapply(id: BookId): String    = id

object AuthorId:
  def apply(value: String): AuthorId = value
  def unapply(id: AuthorId): String  = id

// ─── Value types with smart constructors ──────────────────────────────────────

/** ISBN-13 validated at construction time. */
final case class ISBN private (value: String)

object ISBN:
  def from(s: String): Either[ValidationError, ISBN] =
    val clean = s.filterNot(_ == '-')
    if clean.length == 13 && clean.forall(_.isDigit) then Right(ISBN(clean))
    else Left(ValidationError(s"'$s' is not a valid ISBN-13"))

/** Publication year must be between 1440 (Gutenberg press) and current year. */
final case class PublishYear private (value: Int)

object PublishYear:
  def from(year: Int): Either[ValidationError, PublishYear] =
    if year >= 1440 && year <= 2100 then Right(PublishYear(year))
    else Left(ValidationError(s"$year is not a valid publication year"))

// ─── Enumerations ──────────────────────────────────────────────────────────────

enum Genre:
  case Fiction, NonFiction, SciFi, Mystery, Programming, Biography, History

enum Language:
  case English, Thai, Japanese, French, German

// ─── Domain entities ──────────────────────────────────────────────────────────

final case class Author(
    id: AuthorId,
    name: String,
    nationality: String,
    language: Language = Language.English
)

final case class Book(
    id: BookId,
    title: String,
    authorId: AuthorId,
    isbn: ISBN,
    genre: Genre,
    year: PublishYear,
    available: Boolean = true,
    rating: Option[Double] = None
)

// ─── Error hierarchy (sealed ADT) ─────────────────────────────────────────────

sealed trait LibraryError derives CanEqual:
  def message: String

case class BookNotFound(id: BookId)         extends LibraryError:
  def message = s"Book '${BookId.unapply(id)}' not found"

case class AuthorNotFound(id: AuthorId)     extends LibraryError:
  def message = s"Author '${AuthorId.unapply(id)}' not found"

case class BookUnavailable(book: Book)      extends LibraryError:
  def message = s"'${book.title}' is already borrowed"

case class BookAlreadyAvailable(book: Book) extends LibraryError:
  def message = s"'${book.title}' is already in the library"

case class ValidationError(reason: String) extends LibraryError:
  def message = s"Validation failed: $reason"

case class DuplicateISBN(isbn: ISBN)        extends LibraryError:
  def message = s"A book with ISBN '${isbn.value}' already exists"
