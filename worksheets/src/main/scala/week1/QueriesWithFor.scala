package week1

object QueriesWithFor extends App {

  val books = Books.books

  val books2 =
    for (book <- books; author <- book.authors if author startsWith "Bird,")
      yield book.title

  println(books2)

  val books3 =
    for (book <- books if book.title contains "Program")
      yield book.title

  println(books3)

  val books4 = //{
    for {
      book1 <- books
      book2 <- books
      if book1.title < book2.title
      author1 <- book1.authors
      author2 <- book2.authors
      if author1 == author2
    } yield author1
  //}.distinct // If we had a List[Book] instead of Set[Book]

  println(books4)
}
