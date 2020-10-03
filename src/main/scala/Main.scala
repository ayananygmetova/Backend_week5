import io.circe.generic.auto._,  io.circe.syntax._

import scala.io.Source
import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object Main extends App{

  case class Receipt(namePharmacy: String, BIN: String, VAT: Int, number: String, cashBox: String,
                     change: Int,orderNum: Int, cheque: String, cashier: String, products: Array[Product])

  case class Product(id: Int,fullName:String, name:String, amount:Int, price:Double, totalPrice: Double, number: Option[Int],
                     volume: Option[String], weight:Option[String], productType: Option[String])

  val namePharmacy: Regex = ".*ТОО.*".r
  val BIN: Regex = "БИН\\s*\\d*".r
  val VAT: Regex = "НДС Серия.*".r
  val numberR: Regex = " №\\s\\d*".r
  val cashBox: Regex = "Касса\\d*\\-?\\d*".r
  val change: Regex = "Смена\\s?\\d*".r
  val orderNum: Regex = "Порядковый номер чека.*".r
  val cheque: Regex = "Чек.*".r
  val cashier: Regex = "Кассир.*".r


  val id: Regex = "[0-9]\\.$".r
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

  var nameOfPharmacy = ""
  var BINOfPharmacy = ""
  var VATOfPharmacy = ""
  var numberRec = ""
  var cachboxOfPharmacy = ""
  var changeOfPharmacy = ""
  var orderNumRec = ""
  var chequeRec = ""
  var cashierOfPharmacy = ""

  var id_num = 0
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
      if(findFirstMatchIn(namePharmacy, line)!=null) nameOfPharmacy = line
      else if(findFirstMatchIn(BIN, line)!=null) BINOfPharmacy = line.replaceAll("[a-z\\u0400-\\u04FF\\s]", "")
      else if(findFirstMatchIn(VAT,line)!=null) VATOfPharmacy = line.replaceAll("[a-z\\u0400-\\u04FF\\s]", "")
      else if(findFirstMatchIn(numberR,line)!=null) numberRec= line.replaceAll(" № ", "")
      else if(findFirstMatchIn(cashBox,line)!=null) cachboxOfPharmacy =line.replaceAll("[a-z\\u0400-\\u04FF\\s]", "")
      else if(findFirstMatchIn(change,line)!=null) changeOfPharmacy = line.replaceAll("[a-z\\u0400-\\u04FF\\s]", "")
      else if(findFirstMatchIn(orderNum,line)!=null) orderNumRec = line.replaceAll("[a-z\\u0400-\\u04FF]", "")replaceAll("   №", "")
      else if(findFirstMatchIn(cheque,line)!=null) chequeRec = line.replaceAll("Чек №", "")
      else if(findFirstMatchIn(cashier,line)!=null) cashierOfPharmacy = line.replaceAll("Кассир", "")

      else if (ok) {
        ok = false
        productNumber = if (findFirstMatchIn(number, line) != null) Some(findFirstMatchIn(number, line).drop(1).toInt) else None
        productVolume = matching(volume, line)
        productWeight = matching(weight, line)
        productType = if( productVolume.isDefined || productWeight.isDefined) matching(prType, line) else None
        productName = if (findFirstMatchIn(name, line) != null) findFirstMatchIn(name, line) else ""
        productFullName = line
      }
      else if (findFirstMatchIn(id, line) != null) {
        ok = true
        id_num = line.replace(".", "").toInt
      }
      else if (line == "Стоимость") {
        arr.append(Product(id_num,productFullName, productName, productAmount, productPrice, productTotalPrice,
          productNumber, productVolume, productWeight, productType))
      }
      else if (findFirstMatchIn(totalPrice, line) != null)
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
    val receipt = new Receipt(nameOfPharmacy, BINOfPharmacy,VATOfPharmacy.toInt,  numberRec, cachboxOfPharmacy,
      changeOfPharmacy.toInt, orderNumRec.toInt, chequeRec, cashierOfPharmacy,  arr.toArray)
    val json = receipt.asJson.spaces2
    print_Writer.write(json)
    print_Writer.close()
  }
  parsing()

}
