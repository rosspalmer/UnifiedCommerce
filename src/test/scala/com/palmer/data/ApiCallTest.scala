package com.palmer.data

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ApiCallTest extends AnyWordSpec with Matchers {

  "ApiCall generated curl statements" when {

    "using GET" should {

      "return only two words in no option call" in {

        val curlStatement = ApiCall(RestfulType.GetType, "https://www.example.com/").generateCurlStatement

        curlStatement must equal ("curl https://www.example.com/")

      }

      "return single line with single header call" in {

        val curlStatement = ApiCall(RestfulType.GetType, "https://www.example.com/",  Set(Headers("Test: Header")))
          .generateCurlStatement

        curlStatement must equal ("curl -H 'Test: Header' https://www.example.com/")

      }

      "return three lines with header and output bytes call" in {

        val curlStatement = ApiCall(RestfulType.GetType, "https://www.example.com/",
                                    Set(Headers("Test: Header"), OutputBytes("/here.file")))
          .generateCurlStatement

        curlStatement must equal (
          """curl -H 'Test: Header' \
            |--output /here.file \
            |https://www.example.com/""".stripMargin
        )

      }



    }

  }

}
