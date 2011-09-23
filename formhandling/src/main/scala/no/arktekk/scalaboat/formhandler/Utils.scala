package no.arktekk.scalaboat.formhandler

import java.text.{ParsePosition, NumberFormat}

object Utils {

  def isNumber(s: String) = {
    val formatter = NumberFormat.getInstance
    val pos = new ParsePosition(0)
    formatter.parse(s, pos)
    s.length == pos.getIndex
  }

  def isNotEmpty(s: String) = s != null && !s.trim.isEmpty

  def isEmpty(s: String) = s == null || s.trim.isEmpty
}