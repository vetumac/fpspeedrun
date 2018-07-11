package fpspeedrun.foldables

import cats.syntax.foldable._
import cats.{Eval, Foldable}

final case class Filter[F[_], A](fa: F[A], p: A => Boolean)

object Filter {
  implicit def foldable[F[_] : Foldable]: Foldable[Filter[F, ?]] = new Foldable[Filter[F, ?]] {
    override def foldLeft[A, B](fa: Filter[F, A], b: B)(f: (B, A) => B): B =
      fa.fa.foldLeft(b)((b, a) => if (fa.p(a)) f(b, a) else b)

    override def foldRight[A, B](fa: Filter[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fa.foldRight(lb)((a, bev) => if (fa.p(a)) f(a, bev) else bev)
  }
}

final case class Map[F[_], A, B](fa: F[A], p: A => B)

object Map {
  implicit def foldable[F[_] : Foldable, X]: Foldable[Map[F, X, ?]] = new Foldable[Map[F, X, ?]] {
    override def foldLeft[A, B](fa: Map[F, X, A], b: B)(f: (B, A) => B): B =
      fa.fa.foldLeft(b)((b, a) => f(b, fa.p(a)))

    override def foldRight[A, B](fa: Map[F, X, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fa.foldRight(lb)((a, bev) => f(fa.p(a), bev))
  }
}

final case class Collect[F[_], A, B](fa: F[A], pf: PartialFunction[A, B])

object Collect {
  implicit def foldable[F[_] : Foldable, A]: Foldable[Collect[F, A, ?]] = ???
}

final case class FlatMap[F[_], A, G[_], B](fa: F[A], f: A => G[B])

object FlatMap {
  implicit def foldable[F[_] : Foldable, A, G[_] : Foldable]: Foldable[FlatMap[F, A, G, ?]] = ???
}
