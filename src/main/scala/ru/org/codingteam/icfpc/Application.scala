package ru.org.codingteam.icfpc

object Application extends App {

  // Scary but simply produces (a -> 1, 2, 4), (b -> 3) from ["a", "1", "a", "2", "b", "3", "a", "4")]
  val arguments = args.zipWithIndex
    .groupBy({ case (arg, index) => index / 2 })
    .values
    .map(_.map(_._1))
    .groupBy(_.head)
    .map({case (title, values) => (title, values.flatMap(_.tail))})

  val filename = arguments("-f").head
  val timeLimit = arguments("-t").head
  val memory = arguments("-m").head
  val phrases = arguments("-p")

  println("{}")
}
