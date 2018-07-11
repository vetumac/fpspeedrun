package fpspeedrun.foldables

import cats.implicits._
import cats.{Eval, Foldable}

final case class Iter[F[_]: Foldable, A](x: F[A]) extends Iterable[A]{
  override def iterator: Iterator[A] = x.foldRight(Eval.now(Stream.empty[A])) { (x, acc) => Eval.later(x #:: acc.value) }.value.iterator
}

final case class Fold[A](xs: Iterable[A])

object Fold{
  implicit val foldable: Foldable[Fold] = new Foldable[Fold] {
    override def foldLeft[A, B](fa: Fold[A], b: B)(f: (B, A) => B): B = fa.xs.foldLeft(b)(f)

    override def foldRight[A, B](fa: Fold[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.xs.toStream.foldr(lb)(f)
  }
}


