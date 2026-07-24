package com.roshanp

// ─── Show type class ──────────────────────────────────────────────────────────
// Similar to toString but type-safe and composable.

trait Show[A]:
  def show(a: A): String

object Show:
  def apply[A](using s: Show[A]): Show[A] = s

  /** Convenience constructor from a function. */
  def from[A](f: A => String): Show[A] = a => f(a)

  // ─── Instances ──────────────────────────────────────────────────────────────

  given Show[String]  = s => s
  given Show[Int]     = _.toString
  given Show[Double]  = d => f"$d%.1f"
  given Show[Boolean] = if _ then "yes" else "no"

  given Show[Genre] = _.toString.toLowerCase

  given Show[Language] = _.toString

  given Show[ISBN] = isbn => isbn.value

  given Show[PublishYear] = py => py.value.toString

  given Show[Author] = a =>
    s"${a.name} (${a.nationality}, ${Show[Language].show(a.language)})"

  given Show[Book] = b =>
    val rating  = b.rating.map(r => f" ★$r%.1f").getOrElse("")
    val status  = if b.available then "✓ available" else "✗ borrowed"
    s"[${Show[Genre].show(b.genre)}] \"${b.title}\" (${b.year.value})$rating — $status"

  given [A](using s: Show[A]): Show[Option[A]] =
    case Some(a) => s"Some(${s.show(a)})"
    case None    => "None"

  given [E, A](using se: Show[E], sa: Show[A]): Show[Either[E, A]] =
    case Right(a) => s"Right(${sa.show(a)})"
    case Left(e)  => s"Left(${se.show(e)})"

// ─── Eq type class ────────────────────────────────────────────────────────────

trait Eq[A]:
  def eqv(a: A, b: A): Boolean
  def neqv(a: A, b: A): Boolean = !eqv(a, b)

object Eq:
  def apply[A](using e: Eq[A]): Eq[A] = e
  def from[A](f: (A, A) => Boolean): Eq[A] = (a, b) => f(a, b)
  def fromUniversal[A]: Eq[A] = (a, b) => a == b

  given Eq[String]  = fromUniversal
  given Eq[Int]     = fromUniversal
  given Eq[ISBN]    = (a, b) => a.value == b.value
  given Eq[BookId]   = (a, b) => BookId.unapply(a) == BookId.unapply(b)
  given Eq[AuthorId] = (a, b) => AuthorId.unapply(a) == AuthorId.unapply(b)
  given Eq[Book]     = (a, b) => Eq[BookId].eqv(a.id, b.id)

// ─── Ordering type class ───────────────────────────────────────────────────────

object Ordering:
  given bookByTitle: scala.math.Ordering[Book] =
    scala.math.Ordering.by(_.title.toLowerCase)

  given bookByYear: scala.math.Ordering[Book] =
    scala.math.Ordering.by(_.year.value)

  given bookByRating: scala.math.Ordering[Book] =
    scala.math.Ordering.by[Book, Double](_.rating.getOrElse(0.0)).reverse

// ─── Extension methods ────────────────────────────────────────────────────────

extension [A: Show](a: A)
  def show: String = Show[A].show(a)

extension [A: Eq](a: A)
  def ===(b: A): Boolean = Eq[A].eqv(a, b)
  def =/=(b: A): Boolean = Eq[A].neqv(a, b)
