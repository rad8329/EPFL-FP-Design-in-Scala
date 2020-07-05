package week1

abstract class JSON

case class JSeq(elements: List[JSON]) extends JSON

case class JObj(bindings: Map[String, JSON]) extends JSON

case class JNum(num: Double) extends JSON

case class JStr(str: String) extends JSON

case class JBool(b: Boolean) extends JSON

case object JNull extends JSON

object TestJSON extends App {

  def show(json: JSON): String = json match {
    case JSeq(elements) => "[" + (elements map show mkString ", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\": " + show(value)
      }
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b)  => b.toString
    case JNull     => "null"
  }

  val data = JObj(Map("firstName" -> JStr("Juan"), "lastName" -> JStr("Topo")))

  println(show(data))

  val f: PartialFunction[String, String] = {
    case "ping" => "pong"
  }

  println(f("ping"))
  println(f.isDefinedAt("pingx"))

  val g: PartialFunction[List[Int], String] = {
    case Nil            => "one"
    case x :: y :: rest => "two"
  }

  println(g.isDefinedAt(List(1, 2, 3)))
}
