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

class CSVStore(filename: String){
  def this(filename:String, arr: Array[String]){
    this(filename);
    writeToCSV(arr)
  }

  def path = "src/main/resources/"
  def fullPath = path+filename

  def writeToCSV(writingStuff: Array[String]){
    val pw = new PrintWriter(new File(fullPath))
    writingStuff.foreach( x=>pw.write(x+","))
    pw.close()
  }

  def csvFileToArray() = io.Source.fromFile(fullPath).getLines().next().split(",")

  def changeNumberInFile(number: Double, position: Int): Unit ={
    val f2 = csvFileToArray()
    f2(position) = number.toString
    writeToCSV(f2)
  }

  def removeFile(): Unit = {
    new File(this.fullPath).delete()
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
      val FirstCSV = new CSVStore("f1.csv")
      val SecondCSV = new CSVStore("f2.csv")
      respondWithMediaType(`text/xml`) {
        post {
          import NumberJsonSupport._
          var returning = 0
          entity(as[Number]) { number =>
            val f1 = FirstCSV.csvFileToArray()
            val (returning, result) = Calculator.changeParameter(f1(number.v3).toDouble, number.v2)
            SecondCSV.changeNumberInFile(result, number.v4)
            complete {
              <result>{returning}</result>
            }
          }
        } ~ get {
          parameters('v1.as[Int]) { (v1) =>
            val f2 = SecondCSV.csvFileToArray()
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