package com.palmer.data

import scala.sys.process.*

sealed abstract class RestfulType(val curlPrefixArgs: Seq[String])

object RestfulType {

  case object GetType extends RestfulType(Seq.empty)
  case object PostType extends RestfulType(Seq("-X", "POST"))
  case object PutType extends RestfulType(Seq("-X", "PUT"))
  case object DeleteType extends RestfulType(Seq(""))

}


abstract class ApiCall(val restType: RestfulType, val url: String) {

  def generateCurlStatement: String = {

    val curlOptions: Seq[String] = {

      // Headers -H '<HEADER>'
      this match
        case headers: WithHeaders =>
          headers.getHeaders.values.map(h => s"-H '$h'")
        case _ => Seq.empty[String]
      
    } ++: {
      
      // Input Data -d '<DATA>'
      this match
        case inputData: WithInputData =>
          Seq(s"-d '${inputData.getInputData.value}'")
        case _ => Seq.empty[String]
    
    } ++: {

      // Output Bytes Data --output <FILE>
      this match
        case outputBytes: WithOutputBytes =>
          Seq(s"--output '${outputBytes.getOutputBytes.outputPath}'")
        case _ => Seq.empty[String]

    }

    // Use curl option string to produce human friendly command
    val curlOptionsString = curlOptions.size match
      case 0 => " "
      case 1 => s" ${curlOptions.head} "
      case _ => " " + curlOptions.mkString(" \\\n")

    "curl" + curlOptionsString + url

  }

}

object ApiCall {

  def generate(restType: RestfulType, url: String, options: Set[ApiCallOption] = Set.empty): ApiCall = {

    def getOption(optionKey: String): Option[ApiCallOption] = options.find(_.getOptionKey == optionKey)
    val headers = getOption(OptionKeys.HEADERS)
    val outputBytes = getOption(OptionKeys.OUTPUT_BYTES)
    val inputData = getOption(OptionKeys.INPUT_DATA)

    // TODO make runtime class construction process generic to allow more options
    (headers, outputBytes, inputData) match

      case (Some(h), None, None) => new ApiCall(restType, url) with WithHeaders {
        override def getHeaders: Headers = h.asInstanceOf[Headers]
      }
      case (None, Some(o), None) => new ApiCall(restType, url) with WithOutputBytes {
        override def getOutputBytes: OutputBytes = o.asInstanceOf[OutputBytes]
      }
      case (None, None, Some(d)) => new ApiCall(restType, url) with WithInputData {
        override def getInputData: InputData = d.asInstanceOf[InputData]
      }

      case (Some(h), Some(o), None) => new ApiCall(restType, url) with WithHeaders with WithOutputBytes {
        override def getHeaders: Headers = h.asInstanceOf[Headers]
        override def getOutputBytes: OutputBytes = o.asInstanceOf[OutputBytes]
      }
      case (Some(h), None, Some(d)) => new ApiCall(restType, url) with WithHeaders with WithInputData {
        override def getHeaders: Headers = h.asInstanceOf[Headers]
        override def getInputData: InputData = d.asInstanceOf[InputData]
      }
      case (None, Some(o), Some(d)) => new ApiCall(restType, url) with WithOutputBytes with WithInputData {
        override def getOutputBytes: OutputBytes = o.asInstanceOf[OutputBytes]
        override def getInputData: InputData = d.asInstanceOf[InputData]
      }

      case (Some(h), Some(o), Some(d)) => new ApiCall(restType, url) with WithHeaders with WithOutputBytes
        with WithInputData {
        override def getHeaders: Headers = h.asInstanceOf[Headers]
        override def getOutputBytes: OutputBytes = o.asInstanceOf[OutputBytes]
        override def getInputData: InputData = d.asInstanceOf[InputData]
      }

      case _ => new ApiCall(restType, url) {}

  }

}

trait ApiCallOption {
  def getOptionKey: String
  override def equals(obj: Any): Boolean = super.equals(obj)
  override def hashCode(): Int = super.hashCode()
}
object OptionKeys {
  val HEADERS = "headers"
  val OUTPUT_BYTES = "output_bytes"
  val INPUT_DATA = "input_data"
}

case class Headers(values: Seq[String]) extends ApiCallOption {
  override def getOptionKey: String = OptionKeys.HEADERS
}
trait WithHeaders {
  def getHeaders: Headers
}

case class OutputBytes(outputPath: String) extends ApiCallOption {
  override def getOptionKey: String = OptionKeys.OUTPUT_BYTES
}
trait WithOutputBytes {
  def getOutputBytes: OutputBytes
}

case class InputData(value: String) extends ApiCallOption {
  override def getOptionKey: String = OptionKeys.INPUT_DATA
}
trait WithInputData {
  def getInputData: InputData
}
