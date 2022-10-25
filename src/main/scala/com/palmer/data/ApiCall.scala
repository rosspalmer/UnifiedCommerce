package com.palmer.data

import scala.sys.process.*


case class ApiCall(restType: RestfulType, url: String, options: Set[ApiCallOption]) {

  def generateCurlStatement: String = {

    val fullOptions = options.map(_.generateFullArgument)
    
    // Use curl option string to produce human friendly command
    val curlOptionsString = fullOptions.size match
      case 0 => " "
      case 1 => s" ${fullOptions.head} "
      case _ => " " + fullOptions.mkString(" \\\n") +" \\\n"

    "curl" + curlOptionsString + url

  }

}


sealed abstract class RestfulType(val curlPrefixArgs: Seq[String])

object RestfulType {

  case object GetType extends RestfulType(Seq.empty)
  case object PostType extends RestfulType(Seq("-X", "POST"))
  case object PutType extends RestfulType(Seq("-X", "PUT"))
  case object DeleteType extends RestfulType(Seq(""))

}

trait ApiCallOption {

  def allowsMultiple: Boolean
  def getArgument: String
  def getValue: String
  def getQuoteCharacter: Option[String]

  def generateFullArgument = s"$getArgument ${getQuoteCharacter.getOrElse("")}$getValue${getQuoteCharacter.getOrElse("")}"

  override def equals(obj: Any): Boolean = {
    obj match
      case option: ApiCallOption => getArgument == option.getArgument && getValue == option.getValue
      case _ => false
  }
  override def hashCode(): Int = (getArgument, getValue).hashCode()

}

case class Headers(value: String) extends ApiCallOption {
  override def allowsMultiple: Boolean = true
  override def getArgument: String = "-H"
  override def getValue: String = value
  override def getQuoteCharacter: Option[String] = Some("'")
}

case class OutputBytes(outputPath: String) extends ApiCallOption {
  override def allowsMultiple: Boolean = false
  override def getArgument: String = "--output"
  override def getValue: String = outputPath
  override def getQuoteCharacter: Option[String] = None
}

case class InputData(dataPacket: String) extends ApiCallOption {
  override def allowsMultiple: Boolean = false
  override def getArgument: String = "-d"
  override def getValue: String = dataPacket
  override def getQuoteCharacter: Option[String] = Some("'")
}
