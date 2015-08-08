package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.{OutputDef, FieldDef}

import scala.concurrent.duration._
import scala.io.Source

case class Arguments(fields: Seq[FieldDef],
                     timeLimit: Duration,
                     memoryLimit: Int,
                     cores: Int,
                     phrases: Set[String],
                     print: Boolean)

object Application extends App {

  def parseArgs(): Arguments = {
    // Scary but simply produces (a -> 1, 2, 4), (b -> 3) from ["a", "1", "a", "2", "b", "3", "a", "4")]
    val arguments = args.zipWithIndex
      .groupBy({ case (arg, index) => index / 2 })
      .values
      .map(_.map(_._1))
      .groupBy(_.head)
      .map({case (title, values) => (title, values.flatMap(_.tail))})

    val filenames = arguments("-f")
    val timeLimit = Integer.parseInt(arguments.get("-t").map(_.head).getOrElse("3600"))
    val memory = Integer.parseInt(arguments.get("-m").map(_.head).getOrElse("16348"))
    val cores = Integer.parseInt(arguments.get("-c").map(_.head).getOrElse("8"))
    val phrases = arguments.getOrElse("-p", List()).toList
    val print = arguments.get("-print")

    val fields = (filenames map parseFile).toVector
    Arguments(fields, timeLimit.seconds, memory, cores, Set(phrases: _*), print.isDefined)
  }

  def parseFile(fileName: String): FieldDef = {
    val content = Source.fromFile(fileName).getLines().mkString("\n")
    Serializer.deserialize(content)
  }

  def solve(field: FieldDef, phrases: Set[String], print: Boolean): OutputDef = {
    if (print) {
      MapPrinter.printMap(field)
    }

    OutputDef(0, 0, "", "")
  }

  val arguments = parseArgs()
  arguments.fields foreach { field =>
    val solution = solve(field, arguments.phrases, arguments.print)
    println(Serializer.serialize(solution) + "\n")
  }
}
