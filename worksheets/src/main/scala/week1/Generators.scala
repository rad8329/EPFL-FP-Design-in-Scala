package week1

object Generators extends App {
  trait Generator[+T] {
    self =>

    def generate: T

    def map[U](f: T => U): Generator[U] = new Generator[U] {
      override def generate: U = f(self.generate)
    }

    def flatMap[U](f: T => Generator[U]): Generator[U] = new Generator[U] {
      override def generate: U = f(self.generate).generate
    }
  }

  val integers: Generator[Int] = new Generator[Int] {
    val rand = new java.util.Random

    override def generate: Int = rand.nextInt
  }

  def lists: Generator[List[Int]] =
    for {
      isEmpty <- for (x <- integers) yield x > 0
      list <- if (isEmpty) emptyLists else nonEmptyLists
    } yield list

  def emptyLists: Generator[Nil.type] = single(Nil)

  def nonEmptyLists =
    for {
      head <- integers
      tail <- lists
    } yield head :: tail

  def pairs[T, U](t: Generator[T], u: Generator[U]) =
    for {
      x <- t
      y <- u
    } yield (x, y)

  def single[T](s: T): Generator[T] = new Generator[T] {
    override def generate: T = s
  }

  def choose(from: Int, to: Int): Generator[Int] =
    for (x <- integers) yield from + Math.abs(x) % (to - from)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  println(integers)

  println(lists)
}
