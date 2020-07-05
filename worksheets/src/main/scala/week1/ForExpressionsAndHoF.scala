package week1

object ForExpressionsAndHoF extends App {

  // Always the operations map, flatMap and filter can be expressed with for, in fact, when the compiler compile
  // a for expression it transform that in an HoF like map, flatMap or filter
  def mapFun[T, U](xs: List[T], f: T => U): List[U] = for (x <- xs) yield f(x)

  def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] =
    for (x <- xs; y <- f(x)) yield y

  def filter[T](xs: List[T], f: T => Boolean): List[T] =
    for (x <- xs if f(x)) yield x

  val books = Books.books

  val books1: Set[String] =
    for (book <- books; a <- book.authors if a startsWith "Bird")
      yield book.title

  println(books1)

  val books1_1: Set[String] = books.flatMap(
    book =>
      for (author <- book.authors if author startsWith "Bird") yield book.title
  )

  println(books1_1)

  val books1_2: Set[String] = books.flatMap(
    book =>
      for (author <- book.authors.withFilter(a => a startsWith "Bird"))
        yield book.title
  )

  println(books1_2)

  val books1_3: Set[String] = books.flatMap(
    book =>
      book.authors
        .withFilter(author => author startsWith "Bird")
        .map(_ => book.title)
  )

  println(books1_3)
}
