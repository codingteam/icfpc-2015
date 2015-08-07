package ru.org.codingteam.icfpc

import scala.io.Source

object Application extends App {

  // Scary but simply produces (a -> 1, 2, 4), (b -> 3) from ["a", "1", "a", "2", "b", "3", "a", "4")]
  val arguments = args.zipWithIndex
    .groupBy({ case (arg, index) => index / 2 })
    .values
    .map(_.map(_._1))
    .groupBy(_.head)
    .map({case (title, values) => (title, values.flatMap(_.tail))})

  val filename = arguments("-f").head
  val timeLimit = arguments.get("-t").map(_.head).getOrElse(Int.MaxValue)
  val memory = arguments.get("-m").map(_.head).getOrElse(Int.MaxValue)
  val phrases = arguments.get("-p")
  val print = arguments.get("-print")

  val content = Source.fromFile(filename).getLines().mkString("\n")
  val field = Serializer.deserialize(content)
  if (print.isDefined) {
    MapPrinter.printMap(field)
  }

  println("{}")
}
