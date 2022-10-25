package com.palmer.data
package com.palmer.data

import scala.sys.process.*

sealed abstract class RestfulType(val curlPrefixArgs: Seq[String])

object RestfulType {

  case object GetType extends RestfulType(Seq.empty)
  case object PostType extends RestfulType(Seq("TODO"))
  case object PutType extends RestfulType(Seq("-X", "PUT"))
  case object DeleteType extends RestfulType(Seq(""))

}

// General arguments
// -H headers
// --output <FILE> return bytes
// -d <DATA> input data
// -v include header information

abstract class ApiCall( val restType: RestfulType, val url: String)

object ApiCall {

  def generate(restType: RestfulType, url: String, options: Option[Set[ApiCallOption]] = None): ApiCall = {

    def getOption(optionKey: String): Option[ApiCallOption] = {
      options.getOrElse(Seq.empty).find(_.getOptionKey == optionKey)
    }

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


case class ApiCallResult(apiCall: ApiCall, returnCode: Int, result: Option[String])
//
//case class ApiCallFuture(apiCall: ApiCall) {
//
//  def run(): ApiCallResult = {
//
//    val command: Seq[String] = Seq("curl", "-v") :++ apiCall.callType.curlPrefixArgs :++
//      apiCall.headers.map(h => "-H" +: h).getOrElse(Seq.empty) :++
//      apiCall.data.map(d => Seq("-d", d)).getOrElse(Seq.empty)
//
//    val lazyReturn = command
//    lazyReturn.redu
//    ???
//
//  }
//
//}
