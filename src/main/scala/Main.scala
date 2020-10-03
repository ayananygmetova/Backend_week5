import io.circe.generic.auto._,  io.circe.syntax._

import scala.io.Source
import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object Main extends App{

  case class Product(fullName:String, name:String, amount:Int, price:Double, totalPrice: Double, number: Option[Int],
                     volume: Option[String], weight:Option[String], productType: Option[String])

  val index: Regex = "[0-9]\\.$".r
  val name: Regex = "((\\[.*?])*[a-zA-Z _.!()+=`\"@$#*-]*[a-z\\u0400-\\u04FF]+[a-zA-Z _.!()+=`\"@$#*-]*)+(\\d(\\,\\d)?\\%)?(\\d\\в\\d[a-z\\u0400-\\u04FF]*)?".r
  val amountAndPrice = """(\d+\,0{3}\b) x (\d*\s?\d*\,?0{2}$)""".r("amount","price")
  val totalPrice: Regex = """(^\d*\s?\d*\,?0{2}$)""".r
  val number: Regex = "№[0-9]+".r
  val volume: Regex = "[0-9]+\\s?мл".r
  val weight: Regex = "[0-9]+\\s?мг".r
  val prType: Regex = "(\\d{0,3}\\-?\\s?[a-z\\u0400-\\u04FF\\s]{3,10}[a-zA-Z _.!()]*[^мл])$".r

  val fname = "raw.txt"
  val fSource = Source.fromFile(fname)

  var ok = false
  var arr = ArrayBuffer[Product]()

  var productFullName = ""
  var productName = ""
  var productPrice:Double = 0
  var productAmount = 0
  var productTotalPrice:Double = 0
  var productNumber: Option[Int] = None
  var productVolume: Option[String] = None
  var productWeight: Option[String] = None
  var productType:Option[String] = None

  def findFirstMatchIn(regex: Regex, line:String):String={
    regex.findFirstMatchIn(line) match {
      case Some(result) => result.toString
      case None =>  null
    }
  }
  def matching(regex:Regex, line:String): Option[String]={
    if (findFirstMatchIn(regex, line)==null) return None
    Some(findFirstMatchIn(regex, line))
  }

  def parsing() {

    for (line <- fSource.getLines) {
      if (ok) {
        ok = false
        productNumber = if (findFirstMatchIn(number, line) != null) Some(findFirstMatchIn(number, line).drop(1).toInt) else None
        productVolume = matching(volume, line)
        productWeight = matching(weight, line)
        productType = if( productVolume.isDefined || productWeight.isDefined) matching(prType, line) else None
        productName = if (findFirstMatchIn(name, line) != null) findFirstMatchIn(name, line) else ""
        productFullName = line
      }
      if (findFirstMatchIn(index, line) != null) ok = true
      if (line == "Стоимость") {
        arr.append(Product(productFullName, productName, productAmount, productPrice, productTotalPrice,
          productNumber, productVolume, productWeight, productType))
      }
      if (findFirstMatchIn(totalPrice, line) != null)
        productTotalPrice = line.dropRight(3).replace(" ", "").toDouble

      val result = amountAndPrice.findFirstMatchIn(line)
      if (result.isDefined) {
        val regex = result.get
        val amount = regex.group("amount")
        val price = regex.group("price")
        productPrice = price.dropRight(3).replace(" ", "").toDouble
        productAmount = amount.dropRight(4).toInt
      }
    }
    fSource.close()
    val file_Object = new File("json.txt" )
    val print_Writer = new PrintWriter(file_Object)
    for (i<-0 until arr.length) {
      val json = arr(i).asJson.spaces2
      print_Writer.write(json)
      if (i!=arr.length-1) print_Writer.write(",")
    }
    print_Writer.close()
  }
  parsing()

}
