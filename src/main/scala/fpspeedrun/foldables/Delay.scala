package fpspeedrun.foldables

import cats.Eval
import newts.Dual
import simulacrum.typeclass

@typeclass
trait Delay[A] {
  def delay(x: => A): A
}

sealed trait EvalDelayInstance{
  implicit def evalDelay[A]: Delay[Eval[A]] = (x) => Eval.defer(x)
}
object Delay extends EvalDelayInstance {
  implicit def dual[A](implicit del: Delay[A]): Delay[Dual[A]] = new Delay[Dual[A]] {
    override def delay(x: => Dual[A]): Dual[A] = Dual(del.delay(x.getDual))
  }
}


