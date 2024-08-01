package com.example.movieratings

import org.apache.commons.csv.CSVRecord

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}


case class Movie(id: Short, year: Short, title: String)

object Movie {
  def unapply(csvRecord: CSVRecord): Option[(Short, Short, String)] = {
    val values = csvRecord.values()
    Try(values(0).toShort, values(1).toShort, values(2)) match {
      case Failure(exception) =>
        println(s"Can't parse csv record: ${values.mkString("[", ", ", "]")}, because of $exception")
        None
      case Success(parsed) =>
        Some(parsed)
    }
  }
}

case class Rating(customerId: Int, rate: Byte, date: LocalDate)

object Rating {
  def unapply(csvRecord: CSVRecord): Option[(Int, Byte, LocalDate)] = {
    val Array(customerId, rate, date) = csvRecord.values()
    val dateAsDate = LocalDate.parse(date, DateTimeFormatter.ISO_LOCAL_DATE)
    Some(customerId.toInt, rate.toByte, dateAsDate)
  }
}


object ReportGenerator {

  def main(args: Array[String]): Unit = {
    // check out CsvUtils for methods to read and write CSV files

     val Array(movieTitleFileName, ratingsDirName, reportFileName) = args

    val movies = CsvUtils.readFromFileAsList(new File(movieTitleFileName))
      .collect { case Movie(id, year, title) if year >= 1970 && year <= 1990 =>
        Movie(id, year, title)
      }

    val reportEntries = movies
      .par
      .flatMap { case Movie(id, year, title) =>
        val ratingFile = new File(f"$ratingsDirName/mv_$id%07d.txt")
        val movieRatings = CsvUtils.readFromFileAsList(ratingFile).tail.map {
          case Rating(customerId, rate, date) => rate
        }

        val reviewsNumber = movieRatings.length
        if (reviewsNumber > 1000) {
          val avgRate = movieRatings.foldLeft(0L)(_ + _).toDouble / reviewsNumber
          Some(
            (title, year, avgRate, reviewsNumber)
          )
        } else {
          None
        }
      }
      .seq
      .sortBy {
        case (title, year, rating, reviewsNumber) => (- rating, title)
      }

    CsvUtils.writeToFile(reportEntries.map(re => re.productIterator.toList), new File(reportFileName))
  }

}
