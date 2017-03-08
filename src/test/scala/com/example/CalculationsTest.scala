package com.example

import java.io.File
import org.scalatest.FlatSpec

/*
classes:
CSVStore
should
a) return existing files from pathes
b1)
b) transform file to array of nums in correct way
c) change number on specific position in file correctly
Calculator
should
a) return parameter correctly
b) set parameter correctly

 */
class CalculationsTest extends FlatSpec{

  val FirstCSV = new CSVStore("f1.csv")
  val SecondCSV = new CSVStore("f2.csv")
  val TempCSV = new CSVStore("temp.csv")

  implicit class FileMonads(f: File) {
    def check = f.exists
    def size = f.length()
  }
  def randomArray(size: Int) = {
    var r = new scala.util.Random
    1 to size map { _ =>
      r.nextDouble().toString
    }
  }
  def randomBoundedArray(size: Int, upperBound: Int) = {
    var r = new scala.util.Random
    1 to size map { _ =>
      r.nextInt(upperBound).toDouble
    }
  }

  "Calculator" should "return parameter correctly" in {
    val bigArray = randomBoundedArray(100, 19)
    bigArray.foreach(x=>
      assert(
        Calculator.getParameter(x)<=10))
  }
  it should "set parameter correctly" in {
    val bigArray = randomBoundedArray(100, 9)
    val bigSecondArray = randomBoundedArray(100, 9)
    (0 to 99).foreach(x => {
      val (_, result) = Calculator.changeParameter(bigArray(x), bigSecondArray(x))
      assert(result < 20 && result >= 10)
    }
    )

  }

  var testArray = randomArray(15).toArray
  "CVSStore" should "return exists files from pathes" in {
    assert(new File(FirstCSV.fullPath).check)
    assert(new File(SecondCSV.fullPath).check)
  }
  it should "write array to file" in {
    TempCSV.writeToCSV(testArray)
    assert(new File("temp.csv").size > 0)
  }
  it should "transform csv file to double array" in {
    val isTestArray = TempCSV.csvFileToArray()
    assert(testArray.sameElements(isTestArray))
  }
  it should "change number on specific position" in {
    TempCSV.changeNumberInFile(127.0, 2)
    val fileArray = TempCSV.csvFileToArray()
    testArray(2) = 127.0.toString
    assert(testArray.sameElements(fileArray))
  }
}
