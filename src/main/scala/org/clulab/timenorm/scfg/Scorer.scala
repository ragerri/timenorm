import org.clulab.timenorm.scfg._
import scala.util.{Success, Failure}, scala.io.Source
import java.io._

object Scorer {
  /**
  * This program normalizes the timexes from documents in a directory and
  * compares them to the gold standard normalizations.
  * For now, it does not compare the timex type (DATE, TIME, DURATION and SET).
  */

  def main(lang:String, inDir:String, outDir:String) {
    // Enter the language ("es"/"en") and the input and output paths as arguments.

    // Indicate the names for the gold standard, normalization and error files.
    // Input files shoud be in "{expression}\t{type}\t{normalization}\n" format.
    val goldOutFile = "%s/gold-standard.txt".format(outDir)
    val normOutFile = "%s/normalizations.txt".format(outDir)
    val errorFile = "%s/errors.txt".format(outDir)

    val errorWriter=new PrintWriter(new File(errorFile))

    // Process the input files to get the gold standard and the normalizations.
    getGoldStandard(inDir, goldOutFile)
    getNormalizations(lang, inDir, normOutFile)

    // Turn both files to lists of lines.
    val goldList = Source.fromFile(goldOutFile).getLines.toList
    val normList = Source.fromFile(normOutFile).getLines.toList

    var sumGold = 0
    var sumNorm = 0

    // Compare each pair in the zipped list.
    for ( (goldTimex, normTimex) <- goldList zip normList ) {
      sumGold += 1
      val gold = goldTimex.split("\t")
      val norm = normTimex.split("\t")
      // In case there is a normalization, this can be correct or incorrect.
      if (norm.length == 2) {
        // If both normalizations are the same, sum 1 to sumNorm.
        if ( gold(1) == norm(1) ) { sumNorm += 1 }
        // If not, write the error in errorFile.
        else { errorWriter.write("%s | %s\n".format(goldTimex, norm(1))) }
      }
      // If there is no normalization, write the timex in errorFile.
      else { errorWriter.write("%s\n".format(goldTimex)) }
    }

    errorWriter.close()

    var sumErrors = sumGold - sumNorm
    var accuracy = sumNorm.toFloat / sumGold

    // Print the final results and the list of mistaken timexes.
    println(f"""\n
                |Number of timexes:         $sumGold
                |Correct normalizations:    $sumNorm
                |Incorrect normalizations:  $sumErrors
                |Accuracy:                  $accuracy\n""".stripMargin)
  }

  def getGoldStandard(inDir:String, goldOutFile:String) {
    val goldWriter=new PrintWriter(new File(goldOutFile))
    // Consider every gold standard file.
    for (file <- new java.io.File(inDir).listFiles) {
      // Keep and write only the expressions and their normalizations.
      val timexList = Source.fromFile(file).getLines.toList
      for (timex <- timexList) {
        val expression = timex.split("\t").head
        val normalization = timex.split("\t").last
        goldWriter.write("%s\t%s\n".format(expression, normalization))
      }
    }
    goldWriter.close()
  }

  def getNormalizations(lang:String, inDir:String, normOutFile:String) {

    val normWriter=new PrintWriter(new File(normOutFile))

    // Select the parser for the desired grammar.
    val parser = lang match {
      case "es" => TemporalExpressionParser.es
      case "en" => TemporalExpressionParser.en
    }

    // Consider every file of the directory whose timexes will be normalized.
    for (file <- new java.io.File(inDir).listFiles) {

      // Get the temporal anchor from the normalization of the first line.
      val dctString = Source.fromFile(file).getLines.toList.head.split("\t").last

      val pattern = "T".r
      val anchor = pattern.findFirstIn(dctString) match {
        // Get the anchor if it specifies the time.
        case Some(_) =>
          val dct = dctString.split("T")
          val date = dct(0).split("-")
          val time = dct(1).split(":")
          val year = date(0).toInt
          val month = date(1).toInt
          val day = date(2).toInt
          val hours = time(0).toInt
          val minutes = time(1).toInt
          val seconds = time.length match {
            case 3 => time(2).toInt
            case _ => 0
          }
          TimeSpan.of(year, month, day, hours, minutes, seconds)

        // Get the anchor if it does not specify the time.
        case None =>
          val dct = dctString.split("-")
          val year = dct(0).toInt
          val month = dct(1).toInt
          val day = dct(2).toInt
          TimeSpan.of(year, month, day)
      }

      // Get and write the normalizations of all the timexes in the file.
      val timexList = Source.fromFile(file).getLines.toList
      for (timex <- timexList) {
        // Get and write the expression and its normalization.
        val expression = timex.split("\t").head
        parser.parse(expression, anchor) match {
          // If the parser fails, write only the expression.
          case Failure(temporal) =>
            println("%s".format(expression))
            normWriter.write("%s\n".format(expression))
          // If the parser successes, write the expression and normalization.
          case Success(temporal) =>
            val normalization = temporal.timeMLValue
            println("%s\t%s".format(expression, normalization))
            normWriter.write("%s\t%s\n".format(expression, normalization))
        }
      }
    }
    normWriter.close()
  }

}
