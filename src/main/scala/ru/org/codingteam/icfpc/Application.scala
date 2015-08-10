package ru.org.codingteam.icfpc

import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Date

import ru.org.codingteam.icfpc.definitions.{OutputDef, FieldDef}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.io.Source

case class Arguments(fields: Seq[FieldDef],
                     timeLimit: Duration,
                     memoryLimit: Int,
                     cores: Int,
                     phrases: Set[String],
                     outFile : Option[String],
                     print: Boolean,
                     scoresFile : Option[String])

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
    val scoresFile = arguments.get("-scores").map(_.head)
    val outFile = arguments.get("-output").map(_.head)

    val fields = (filenames map parseFile).toVector
    Arguments(fields, timeLimit.seconds, memory, cores, Set(phrases: _*), outFile, print.isDefined, scoresFile)
  }

  def parseFile(fileName: String): FieldDef = {
    val content = Source.fromFile(fileName).getLines().mkString("\n")
    Serializer.deserialize(content)
  }

  def solve(field: FieldDef, phrases: Set[String], print: Boolean, mbScores : Option[String]): Seq[OutputDef] = {
    if (print) {
      MapPrinter.printMap(field)
    }

    var good = new ListBuffer[(Int, Int, Int)]()
    val outs = field.sourceSeeds map { seed =>
      val commands = Strategist.solution(field, seed, phrases)
      mbScores match {
        case Some(scoresFile) =>
          val oldScores = Utils.getScore(scoresFile, field.id, seed)
          val maxOldScore = if (oldScores.isEmpty) 0 else oldScores.max
          val emulator = new Emulator(Field.from(field), phrases)
          emulator.load(field)
          emulator.initSourceWithSeed(seed)
          emulator.emulate(commands)
          val newScore = emulator.score
          println(s"Seed $seed: Maximum old score: $maxOldScore; new score: $newScore")
          if (newScore > maxOldScore) {
            good += ((seed, maxOldScore, newScore))
          }

        case None =>
      }
      val tag = s"seed$seed@" + new SimpleDateFormat("HH:mm:z").format(new Date())
      OutputDef(field.id, seed, tag, Utils.encode(commands))
    }
    if (!good.isEmpty && mbScores.isDefined) {
      println("Better solutions:")
      good.map(s =>
        println(s"  Seed ${s._1}: was ${s._2}, new ${s._3}")
      )
      outs.filter(out => good.map(_._1).contains(out.seed))
    } else {
      outs
    }
  }

  val arguments = parseArgs()
  arguments.fields foreach { field =>
    val solutions = solve(field, arguments.phrases, arguments.print, arguments.scoresFile)
    val output = arguments.outFile match {
      case None => System.out
      case Some(path) => new PrintStream(path)
    }
    output.println(Serializer.serialize(solutions) + "\n")
  }
}
