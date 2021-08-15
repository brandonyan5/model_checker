package src

import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}

import java.io.{File, FileReader, IOException}
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

/**
 * Object containing a CSV parsing utility method.
 */
object Parser {

  private final val CSV_FORMAT = CSVFormat.RFC4180.withHeader();

  /**
   * Parses a CSV file, calling a user inputted function to handle individual
   * records.
   * @param csvFile the path to the csv file
   * @param parseRecord function to handle each CSV record
   */
  def parseCSV(csvFile: String, parseRecord: CSVRecord => Unit): Unit = {
    try {
      val parser: CSVParser = new CSVParser(new FileReader(new File(csvFile)), CSV_FORMAT)
      val records = parser.getRecords.asScala.toList
      for (record <- records) {
        parseRecord(record)
      }
    } catch {
      case e: IOException => println("Error reading " + csvFile + ":\n" + e.getMessage)
    }
  }
}
