package no.arktekk.scalaboat.formhandler
import Utils._

trait Listeners{
  var onChange:()=>Unit = () => {}
}

abstract class Field[T](var value: Option[T], val p: Form) extends Listeners{

  lazy val name = p.fields.find(fw => fw.field == this).map(_.name).getOrElse("")

  def getName = name

  def merge(s: String)

  private var em: Option[Tuple2[Int, String]] = None

  final def addErrorMessage(m: Tuple2[Int, String]) {
    em match {
      case None => em = Some(m)
      case Some(s) => if (em.get._1 > m._1) em = Some(m)
    }
  }

  final def clearErrorMessage() {
    em = None
  }

  final def errorMessage = em.map(_._2)

  final def getValue: String = toString;


}

class StringField(private var v: Option[String] = None)(implicit p: Form) extends Field[String](v, p) {

  def this(s: String)(implicit p: Form) = this (if (s == null || s.eq("")) None else Some(s))

  override def merge(s: String) {
    value = if (s == null || s.eq("")) None else Some(s)
  }

  override def toString = value.getOrElse("")
}

class IntField(private var v: Option[Int] = None)(implicit p: Form) extends Field[Int](v, p) {
  def this(i: Int)(implicit p: Form) = this (Some(i))

  override def merge(s: String) {
    value = if (isEmpty(s) || !isNumber(s)) None else Some(s.toInt)
  }

  override def toString = value.map(_.toString).getOrElse("")
}

class SelectField(private var v: Option[String] = None, var options: List[SelectItem] = List())(implicit p: Form) extends Field[String](v, p) with SelectItems with DisplayText {

  def getOptions = (List(SelectItem.defaultOption) ++ options).toArray

  override def getDisplayText = {
    options.find(o => o.value.equals(value.getOrElse("**N/A**")))
    match {
      case None => ""
      case Some(s) => s.description
    }
  }

  override def merge(s: String) {
    value = if (s == null || s.eq("")) None else Some(s)
  }

  override def toString = value.map(_.toString).getOrElse("")
}

class RadioField(private var v: Option[String] = None, var options: List[SelectItem] = List())(implicit p: Form) extends Field[String](v, p) with SelectItems {
  override def getOptions = options.toArray
}

class DisplayTextField(private var v: Option[String] = None)(implicit p: Form) extends Field[String](v, p) with DisplayText {
  var displayText = "";

  override def merge(s: String) {
    value = if (s == null || s.eq("")) None else Some(s)
  }

  override def getDisplayText = displayText

  override def toString = value.getOrElse("")
}

class FieldWrapper(val name: String, val value: Option[_], val field: Field[_]) {
  override def toString = "Field: " + name + ", " + value + ", " + field.errorMessage
}

case class SelectItem(value: String, description: String) {

  def getValue = value

  def getDescription = description

}

object SelectItem {
  val defaultOption = SelectItem("", "-- Velg --")
  val yesNoOptions = List(SelectItem("true", "Ja"), SelectItem("false", "Nei"))

  def apply(desc: String) = new SelectItem(desc, desc)
}

trait Optional

trait PositiveInteger

trait DisplayText {
  def getDisplayText: String
}

trait SelectItems {
  self: Field[String] =>

  def getOptions: Array[SelectItem]

  override def merge(s: String) {
    value = if (s == null || s.eq("")) None else Some(s)
  }
}
