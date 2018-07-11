package fpspeedrun
package foldables

import cats.syntax.semigroup._
import cats.{Eval, Foldable, Monoid}
import fpspeedrun.syntax.delay._

abstract class FoldMapDelay[F[_]] extends Foldable[F] {
  def foldMapDelay[A, B: Monoid: Delay](fa: F[A])(f: A => B): B

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = ???

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    foldMapDelay[A, LzEndo[B]](fa)(a => LzEndo[B](f(a, _))).run(lb)
}

object FoldMapDelay {

  implicit val streamInstance: Foldable[Stream] = new FoldMapDelay[Stream] {
    override def foldMapDelay[A, B: Monoid : Delay](fa: Stream[A])(f: A => B): B = fa match {
      case Stream.Empty => Monoid[B].empty
      case _ => f(fa.head) |+| foldMapDelay(fa.tail)(f).delay
    }
  }
}
