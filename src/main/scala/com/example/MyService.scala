package com.example

import akka.actor.Actor
import spray.http.MediaTypes._
import spray.http._
import spray.httpx.SprayJsonSupport
import spray.httpx.unmarshalling._
import spray.routing._
import spray.json._
import DefaultJsonProtocol._
import java.io._

class MyServiceActor extends Actor with MyService {

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}

case class Number(v2: Double, v3: Int, v4: Int)

object CSVStore {
  def path = "src/main/resources/"
  def f1 = path + "f1.csv"
  def f2 = path +"f2.csv"

  def writeToCSV(writingStuff: Array[String], filename: String){
    val pw = new PrintWriter(new File(filename))
    writingStuff.foreach( x=>pw.write(x+","))
    pw.close()
  }
  def csvFileToArray(filename: String) = io.Source.fromFile(filename).getLines().next().split(",")
  def changeNumberInFile(filename: String, number: Double, position: Int): Unit ={
    val f2 = csvFileToArray(filename)
    f2(position) = number.toString
    writeToCSV(f2, filename)
  }
}

object Calculator {
  def getParameter(number: Double) = if (number > 10) number - 10 else number

  def changeParameter(f: Double, s: Double) = {
    var returning = 0
    val result = if ((f + s )< 10)
    f + s + 10
    else{
      returning = 1
      f + s
    }
    (returning, result)
  }
}



object NumberJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val NumberFormats = jsonFormat3(Number)
}

trait MyService extends HttpService {

  val myRoute =
    pathPrefix("rest" / "calc") {
      respondWithMediaType(`text/xml`) {
        post {
          import NumberJsonSupport._
          var returning = 0
          entity(as[Number]) { number =>
            val f1 = CSVStore.csvFileToArray(CSVStore.f1)
            var f2 = CSVStore.csvFileToArray(CSVStore.f2)
            val (returning, result) = Calculator.changeParameter(f1(number.v3).toDouble, number.v2)
            CSVStore.changeNumberInFile(CSVStore.f2, result, number.v4)
            complete {
                <result>{returning}</result>
            }
          }
        } ~ get {
          parameters('v1.as[Int]) { (v1) =>
            val f2 = CSVStore.csvFileToArray(CSVStore.f2)
            val number = f2(v1).toDouble
            val result = Calculator.getParameter(number)
            complete {
              <result>{result}</result>
            }
          }
        }
      }
    }
}