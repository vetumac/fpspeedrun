package fpspeedrun

import cats.Eval
import fpspeedrun.foldables.LzEndo

object Hello {
  def main(args: Array[String]): Unit = {
    //    val stream = Iter(Filter(Stream.from(1), (x: Int) => (x % 3) == 0)).iterator.take(1000).toList

    //    print(stream)

    //    print(Fold(Map(1 -> "one", 2 -> "two")).toList)

    //    val st = Stream.from(1).take(10).map(x => Dual(x.toString)).intercalate(Dual(",")).getDual
    //      .foldMap(x => x.toString)

    val st = (1 to 100).foldMap((i: Int) => LzEndo[Int](x => x.map(_ + i)).run(Eval.now(1)))
    //
    print(st)

  }

}
