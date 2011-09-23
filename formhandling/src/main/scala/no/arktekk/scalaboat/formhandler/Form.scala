package no.arktekk.scalaboat.formhandler

import scala.Either
import reflect.BeanProperty
import java.util.{Properties, ArrayList, HashMap}
import java.io.InputStreamReader

abstract class Form {
  implicit val p:Form = this

  final def fields = this.getClass.getDeclaredFields.filter(f => {
    f.setAccessible(true)
    f.get(this).isInstanceOf[Field[_]]
  }).map(f => new FieldWrapper(f.getName, f.get(this).asInstanceOf[Field[_]].value, f.get(this).asInstanceOf[Field[_]]))

  final def subForms = this.getClass.getDeclaredFields.filter(f => {
    f.setAccessible(true)
    f.get(this).isInstanceOf[Form]
  }).map(f => (f.getName, f.get(this).asInstanceOf[Form]))

  final def options = {
    fields.filter(f => f.field.isInstanceOf[SelectField]).foldLeft(Map[String, Array[SelectItem]]())((acc, o) => {
      acc + (o.name -> o.field.asInstanceOf[SelectField].getOptions)
    })
  }

  final def clear() {
    fields.foreach(_.field.value = None)
    subForms.foreach(_._2.clear())
    defaults()
  }

  def defaults(){}

  final def raiseErrorWhen[T](field: Field[T], erroCode: String)(r: (T => Boolean)) = field.value.map(v => if (r(v)) field.addErrorMessage(1, erroCode))

  def validate()(f: => Unit) = {
    fields.foreach(_.field.clearErrorMessage())
    checkMissingRequiredFields()
    checkPositiveIntegers()
    f
    collectErrorMessages
  }

  final def asJavaMap() = {
    new HashMap[String, Any] {
      put("fields",
        fields.foldLeft(new ArrayList[HashMap[String, Any]]())((acc, fld) => {
          acc.add(new HashMap[String, Any] {
            put("name", fld.name)
            fld.field.value match {
              case Some(v) => put("value", v.toString)
              case None =>
            }
            fld.field.errorMessage match {
              case Some(e) => put("errormessage", ErrorCodes.decodeErrorCode(e, fld.name))
              case None =>
            }
            fld.field match {
              case i: DisplayTextField => put("displayText", i.displayText)
              case _ =>
            }
            fld.field match {
              case i: SelectField => put("options", i.getOptions)
              case _ =>
            }
          })
          acc
        })
      )
    }
  }

  final def merge(changes: Array[ChangeSet]): Either[Throwable, Boolean] = {
    changes.filter(!_.field.contains(".")).foreach(c => {
      val field: Option[Field[_]] = fields.find(_.name.equals(c.field)).map(_.field)
      field.map(fld=>{
        fld.merge(c.value)
        fld.onChange()
      })
    })
    subForms.foreach(sf => {
      sf._2.merge(changes.filter(_.field.startsWith(sf._1 + ".")).map(cs => {
        new ChangeSet {
          field = cs.field.substring(sf._1.length() + 1, cs.field.length())
          value = cs.value
        }
      }))
    })
    Right(true)
  }

  private final def checkMissingRequiredFields() {
    fields.filter(f => {
      !f.field.isInstanceOf[Optional]
    }).foreach(f => {
      f.value match {
        case None => f.field.addErrorMessage((0, ErrorCodes.required))
        case Some(s) => Nil
      }
    })
  }

  private final def checkPositiveIntegers() {
    fields.filter(f => {
      f.field.isInstanceOf[PositiveInteger] && f.value.isInstanceOf[Option[_]] && f.value.isDefined
    }).foreach(f => {
      if (f.value.get.asInstanceOf[Int] <= 0) f.field.addErrorMessage(1, ErrorCodes.nonPositiveInteger) else Nil
    })
  }

  private final def collectErrorMessages = fields.foldLeft(Map[String, String]())((acc, x) => x.field.errorMessage.map(e => acc + (x.name -> ErrorCodes.decodeErrorCode(e, x.name))).getOrElse(acc))

}

class Diff {
  @BeanProperty var currentForm: String = ""
  @BeanProperty var changes: Array[ChangeSet] = Array()
}

class ChangeSet {
  @BeanProperty var field: String = ""
  @BeanProperty var value: String = ""
}

object ErrorCodes {

  val msgs = new ErrorCodes

  val required = "required"
  val nonPositiveInteger = "nonPositiveInteger"
  val invalid = "invalid"

  def decodeErrorCode(code: String, field: String) = {
    val lookup = field + "." + code
    msgs.lookup(lookup).getOrElse(msgs.lookup(code).getOrElse(lookup))
  }
}

class ErrorCodes {
  val msgs = new Properties()
  val resource = getClass.getResourceAsStream("/messages/errors.properties")
  msgs.load(new InputStreamReader(resource, "UTF-8"));

  def lookup(code:String) = {
    val msg = msgs.get(code)
    if (msg == null) None else Some(msg.toString)
  }
}
