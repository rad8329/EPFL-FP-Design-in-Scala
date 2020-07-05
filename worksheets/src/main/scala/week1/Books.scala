package week1

object Books {

  case class Book(title: String, authors: List[String])

  val books: Set[Book] = Set(
    Book(
      "Structure and interpretation of computers programs",
      List("Abelson, Harald", "Sussman, Gerald J.")
    ),
    Book(
      "Introduction to Functional Programing",
      List("Bird, Richard", "Wadler, Phil")
    ),
    Book("Effective Java", List("Bloch, Joshua")),
    Book("Effective Java 2", List("Bloch, Joshua")),
    Book("Java Puzzlers", List("Bloch, Joshua", "Gafter, Neil")),
    Book(
      "Programing in Scala",
      List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")
    )
  )
}
