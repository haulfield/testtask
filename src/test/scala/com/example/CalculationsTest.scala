package com.example

import java.io.File
import org.scalatest.FlatSpec
/**
  * Created by Дайдара on 21.02.2017.
  */

class CalculationsTest extends FlatSpec{

  val path = "src/main/resources/"
  implicit class FileMonads(f: File) {
    def check = f.exists
  }

  "Files f1 and f2 " should "exist" in {
    assert(new File(path+"f1.csv").check)
    assert(new File(path+"f2.csv").check)
  }
}
