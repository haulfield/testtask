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
            val f1 = io.Source.fromFile("src/main/resources/f1.csv").getLines().next().split(",")
            var f2 = io.Source.fromFile("src/main/resources/f2.csv").getLines().next().split(",")
            val result = if ((f1(number.v3).toDouble + number.v2 )< 10)
              f1(number.v3).toDouble+number.v2+10
            else{
              returning = 1
              f1(number.v3).toDouble+number.v2
            }
            val pw = new PrintWriter(new File("src/main/resources/f2.csv"))
            f2(number.v4) = result.toString
            f2.foreach { x => pw.write(x+",")}
            pw.close()

            complete {
                <result>{returning}</result>
            }
          }
        } ~ get {
          parameters('v1.as[Int]) { (v1) =>
            val f2 = io.Source.fromFile("src/main/resources/f2.csv").getLines().next().split(",")
            val number = f2(v1).toDouble
            val result = if (number > 10) number - 10 else number
            complete {
              <result>{result}</result>
            }
          }
        }
      }
    }
}