package fpspeedrun.foldables

import cats.{Eval, Monoid}

final case class LzEndo[A](run: Eval[A] => Eval[A]) extends AnyVal

object LzEndo {
  implicit def endoInstance[A]: Monoid[LzEndo[A]] with Delay[LzEndo[A]] =
    new Monoid[LzEndo[A]] with Delay[LzEndo[A]] {
      override def empty: LzEndo[A] = LzEndo(identity)

      override def delay(x: => LzEndo[A]): LzEndo[A] =
        LzEndo(la => Eval.defer(x.run(la)))

      override def combine(x: LzEndo[A], y: LzEndo[A]): LzEndo[A] =
        LzEndo(la => Eval.defer(x.run(Eval.defer(la))))

      //        LzEndo(x.run compose y.run)
    }
}
