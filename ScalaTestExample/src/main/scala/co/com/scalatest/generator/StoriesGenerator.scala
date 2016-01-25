package co.com.scalatest.generator

import java.io.File
import java.io.PrintWriter

import scala.collection.immutable.Stream.consWrapper
import scala.collection.mutable.ListBuffer

object StoriesGenerator {
  
  case class Verificacion(test:String, id:String, detalle:String, detalleAdicional: String)
  
  val separator = "\t"
  
  def extraer(linea:String) = {
    linea.split(separator).toList match {
      case test :: id :: det :: det2  :: rest => 
        Some(Verificacion(test, id, det, det2))
      case test :: id :: det :: rest => 
        Some(Verificacion(test, id, det, ""))
      case test :: id :: rest =>
        Some(Verificacion(test, id, "",""))
      case _ =>
        None
    }
  }
  
  val allOptions = List("Search",
    	"Search Result",
    	"Load Page")
    	
  val loadPage = "Load Page"
  
  val initialStep = List(loadPage)
  
  val step1Step01 = List("Search")
  val step1Step02 = List("Search Result")
  
  val map = Map("Load Page" -> step1Step01,
      "Search Result" -> step1Step02)
      
  val inputDir = """src\main\resources\input"""
  val outputDir = """src\main\resources\generated_user_stories"""
  
  var testFile:String = null  
  var outWriter:PrintWriter = null
    
  // HTML writer

  def printHtml(text:String, bold:Boolean = false, centered: Boolean = false){
    outWriter.println("<tr><td>")
    if(centered) outWriter.print("<center>")
    if(bold) outWriter.print("<strong>")
    outWriter.print(text)
    if(bold) outWriter.print("</strong>")
    if(centered) outWriter.print("</center>")
    outWriter.println("</td></tr>")
  }
    
  def printHtmlTitle(elementName:String) = {
    outWriter.println("""<html><body><header><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></header>""")
    outWriter.println("<h1>" + elementName + "</h1>")
  }
   
  def printHtmlHeader = {
    outWriter.println(s"<table border='1'>")
  }
  
  def printHtmlFooter = {
    outWriter.println("</body></html>")
  }
  
  def validateTemplate(testFile:String) {
        val file = io.Source.fromFile(testFile, "UTF-8").getLines.toList
    val buff = new ListBuffer[Verificacion]
    if(file.head != "ID	VERIFICTION	DETAIL 2 DETAIL	RESULT"){
      printHtmlTitle("<font color='red'>HEADER DOESN'T MATCH</font>")
      printHtmlFooter
      outWriter.close()
      return
    }else for(line <- file.tail){
      extraer(line) match {
        case Some(verification) =>
          buff += verification
        case None =>
          print("There's an error on the line "+file)
      }
    }

    val groups = buff.filter(elem => allOptions.contains(elem.id)).groupBy( _.test)
    
    def printStep(elem:Verificacion) = {
      elem.id match {
        case "Load Page" =>
          printHtml(s"To have the option to load de page ${elem.detalle}.")
        case "Search" =>
          val detalle = if ( elem.detalle.nonEmpty )
            printHtml(s"To have the option of searching ${elem.detalle}.")
          else
            printHtml(s"Al verificar la estructura me sean mostrado el mensaje ${elem.detalle}.")
        case "Search Results" =>
          printHtml(s"Ver los errrores del campo ${elem.detalle}.")
        case _ =>
          throw new IllegalArgumentException(s"The step ${elem.id} is not valid, please correct the step!")
      }
    }
    
    val byUser = groups.groupBy(test => test._1.split("-")(1))
    val elementName = byUser.head._1.split("-")(0)
    for(groupByUser <- byUser){
      printHtmlHeader
      printHtml(groupByUser._1, true, true)
      printHtml("As:", true)
      printHtml(s"A User that likes to search on the web")
      printHtml("I want:", true)
      printHtml("To search on Google.")
      printHtml("For:", true)
      printHtml("Research purposes.")
      printHtml("&nbsp;")
      printHtml("ACCEPTANCE CRITERIA", true, true)
      for(test <- groupByUser._2){
        test match {
        case (key, steps) =>
          key.split("-").toList match {
            case grupo :: usuario :: rest =>
              val archivo = rest.mkString("-")
              printHtml("When", true)
              printHtml(s"The user $usuario, load the page Google.")
              printHtml("Espero", true)
              var entryPoint:Option[String] = None
              var errorCount = 0
              for(elem <- steps){
                elem match {
                  case Verificacion(_, "Load Page" ,_ , _) =>
                    entryPoint match {
                      case Some(value) if (value == elem.id) =>
                        printHtml(s"<font color='orange'>WARNING: the escena '${elem.id}', is duplicate.</font>")
                      case Some(value) =>
                        printHtml(s"<font color='red'>ERROR: You are trying to evaluate the this escena '${elem.id}', and this is diferent respect the evaluate value.'$value'</font>")
                        errorCount += 1
                      case None =>
                        entryPoint = Some(elem.id)
                        printStep(elem)
                    }
                  case Verificacion(_,accionLoadPage,_,_) if initialStep.contains(accionLoadPage) =>
                    validateAndPrint(elem, loadPage)
                  case Verificacion(_,accionSearch,_,_) if (step1Step01).contains(accionSearch) =>
                    validateAndPrint(elem)
                  case Verificacion(_,accionSearchResult,_,_) if (step1Step02).contains(accionSearchResult) =>
                    validateAndPrint(elem)
                  case _ =>
                    printHtml(s"ERROR: The element '${$elem.id}' is not valid.")
                    errorCount += 1
                }
              }
              def validateAndPrint(elem:Verificacion, correctScenario:String = loadPage, incorrectScenario:String="") = {
                entryPoint match {
                  case Some(initialPath) if initialPath == incorrectScenario =>
                    printHtml(s"<font color='red'>ERROR: The action '${elem.id}' is only valid at the escena'$correctScenario'</font>")
                    errorCount += 1 
                  case Some(_) => printStep(elem)
                  case None =>
                    entryPoint = Some(correctScenario)
                    printStep(elem)
                }
              }
              errorCount match {
                case 0 => // O.K.
                case 1 =>
                  printHtml(s"<font color='red'>We found an error during de evaluationg of the item $key</font>")
                case count =>
                  printHtml(s"<font color='red'>We found $count error during de evaluationg of the item $key</font>")
              }
              printHtml("&nbsp;")
            case _ =>
              println("Invalid name: " + key)
          }
        case _ =>
          println("Is there no options?")
        }
      }
      outWriter.println("</table>")
    }
     printHtmlFooter
     outWriter.close()
  }
  
  def getFileTree(f: File): Stream[File] =
        f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) 
               else Stream.empty)
  
  def listarArchivos(inputDir:String) = {
    val buffer = scala.collection.mutable.ListBuffer[String]()
    for(filename <- getFileTree(new File(inputDir)).map(_.getName())){
      if(filename.endsWith(".tsv"))
        buffer += filename.substring(0, filename.length()-4)
    }
    buffer.toList
  }
  
  def main(args:Array[String]) {
    for(item <- listarArchivos(inputDir)){
	      testFile = inputDir + File.separator + s"$item.tsv"
	      
	      val parametersFile = outputDir + File.separator + s"$item.html"
	      print(parametersFile+"\r\n")
	      outWriter = new PrintWriter(parametersFile, "UTF-8")
	      validateTemplate(testFile)
    }
  }
}