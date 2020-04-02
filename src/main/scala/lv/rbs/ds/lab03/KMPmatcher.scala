package lv.rbs.ds.lab03


import net.liftweb.json.JValue

import scala.collection.mutable.ListBuffer

class KMPmatcher(var pattern: String) {

  def getPrefixFun(): List[Int] = {

    var returnlist= new ListBuffer[Int]()

    val x = pattern.length

    var k = 0



    for (z <- 1 to x) {
      returnlist += z }

    returnlist(0) = -1

    returnlist(1) = 0

    for (q <- 2 to x) {

      while (k > 0 && this.pattern(k) != this.pattern(q - 1)) {
        k = returnlist(k)
      }
      if (this.pattern(k) == this.pattern(q - 1)) {

        k += 1
      }
      returnlist(q) = k
    }
    returnlist.toList
  }

  def findAllIn(text: CharSequence): Iterator[Int] = {

    var strSearch = new ListBuffer[Int]()

    val get = getPrefixFun
    var k = 0

    val textlen = text.length
    val prefixlen = this.pattern.length

    for (i <- 0 until textlen) {

      while (k > 0 && this.pattern(k) != text.charAt(i)) {

        k = get(k)
      }
      if (this.pattern(k) == text.charAt(i)) {

        k += 1
      }
      if (k == prefixlen) {

        var prntt = i - prefixlen + 1

        strSearch += prntt
        k = get(k)
      }
    }
    strSearch.toList.iterator
  }

  def toJson(text: CharSequence): String = {
 }
}