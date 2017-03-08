package com.example

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import spray.http.MediaTypes._
import StatusCodes._
import shapeless.~>

class MyServiceSpec extends Specification with Specs2RouteTest with MyService {
  def actorRefFactory = system

  val FirstCSV = new CSVStore("f1.csv")
  val SecondCSV = new CSVStore("f2.csv")
  FirstCSV.writeToCSV(Array(1.1, 2.2, 3.3, 4.4, 5.5, 6.5).map(x=>x.toString))
  SecondCSV.writeToCSV(Array(1.1, 2.2, 3.3, 4.4, 5.5, 6.5).map(x=>x.toString))
  def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)
  "MyService" should {

    "leave GET requests to paths others to /rest/calc/ unhandled" in {
      Get("/") ~> myRoute ~> check {
        handled must beFalse
      }
      Get("/nothing") ~> myRoute ~> check {
        handled must beFalse
      }
      Get("/404") ~> myRoute ~> check {
        handled must beFalse
      }
      Get("/rest/calc/?v1=1") ~> myRoute ~> check {
        handled must beTrue
      }
    }

    "/rest/calc/?v1=0 should return first element of f2 file in XML" in {
      Get("/rest/calc/?v1=0") ~> myRoute ~> check {
        val tempFile = SecondCSV.csvFileToArray()
        val number = tempFile(0).toDouble
        val testResult = if (number > 10) number - 10 else number
        contentType.toString must contain("text/xml")
        responseAs[String] must contain(testResult.toString)
      }
    }

    "post request to /rest/calc/ with v4 parameter as 0 must change the first element of f2 file" in {
      val number = io.Source.fromFile("src/main/resources/f2.csv").getLines().next().split(",")(0).toDouble
      Post("/rest/calc/", HttpEntity(`application/json`,"""{"v2":1,"v3":2,"v4":0}""")) ~>
        myRoute ~> check{
        val secondNumber = io.Source.fromFile("src/main/resources/f2.csv").getLines().next().split(",")(0).toDouble
        number must not equalTo(secondNumber)
      }
    }

    "post request to /rest/calc/ must return 0 or 1 in XML" in {
      Post("/rest/calc/", HttpEntity(`application/json`,"""{"v2":1,"v3":2,"v4":0}""")) ~>
        myRoute ~> check{
        val myResult = scala.xml.XML.loadString(responseAs[String]).text.toInt
        contentType.toString must contain("text/xml")
        myResult must beBetween(0,1)
      }
    }

    "calculation should not be wrong on starting data and 1,2,1 parameters (it should be equal 14.3)" in {
      Post("/rest/calc/", HttpEntity(`application/json`,"""{"v2":1,"v3":2,"v4":1}""")) ~>
        myRoute ~> check{
        val secondNumber = io.Source.fromFile("src/main/resources/f2.csv").getLines().next().split(",")(1).toDouble
        secondNumber must equalTo(14.3)
      }
    }

  }
}
