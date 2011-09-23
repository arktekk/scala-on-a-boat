package no.arktekk.scalaboat.formhandler

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._
import scala.Array
import ErrorCodes._

class FormTest extends AssertionsForJUnit {

  @Test
  def shouldValidateRequiredField() {
    object Form extends Form {
      val field1 = new StringField(None)
      val field2 = new StringField("Something")
    }

    val errors = Form.validate(){}
    assertEquals(1, errors.size)
    assertEquals(decodeErrorCode(required, "field1"), errors.getOrElse("field1", "expected error message not found"))
  }

  @Test
  def shouldIgnoreOptionalFields() {
    object Form extends Form {
      val field1 = new StringField(None)
      val field2 = new StringField(None) with Optional
    }

    val errors = Form.validate() {}
    assertEquals(1, errors.size)
    assertEquals(decodeErrorCode(required, "field1"), errors.getOrElse("field1", "expected error message not found"))
    assertEquals(None, errors.get("field2"))
  }

  @Test
  def shouldNotAllowNonPositiveIntegers() {
    object Form extends Form {
      val field1 = new IntField(Some(0)) with PositiveInteger
      val field2 = new IntField(Some(2)) with PositiveInteger
    }

    val errors = Form.validate() {}
    assertEquals(1, errors.size)
    assertEquals(decodeErrorCode(nonPositiveInteger, "field1"), errors.getOrElse("field1", "expected error message not found"))
    assertEquals(None, errors.get("field2"))
  }

  @Test
  def shouldAllowCustomValidation() {
    object Form extends Form {
      val field1 = new IntField(Some(0))

      override def validate()(f: => Unit) = {
        super.validate() {
          raiseErrorWhen(field1, "T1")(_ <= 10)
        }
      }
    }

    val errors = Form.validate() {}
    assertEquals(1, errors.size)
    assertEquals(decodeErrorCode("T1", "field1"), errors.getOrElse("field1", "expected error message not found"))
    Form.field1.value = Some(20)
    assertEquals(0, Form.validate() {}.size)

  }

  @Test
  def shouldBeAbleToMergeChangeSet() {
    class TestForm extends Form {
      val field1 = new IntField(Some(0))
      val field2 = new StringField("Kaare")
    }
    val form = new TestForm
    assertEquals(Some(0), form.field1.value)
    assertEquals(Some("Kaare"), form.field2.value)
    form.merge(Array(new ChangeSet {
      field = "field1";
      value = "2"
    }))
    assertEquals(Some(2), form.field1.value)
    assertEquals(Some("Kaare"), form.field2.value)
  }

  @Test
  def shouldBeAbleToMergeChangeSetWithChildForms() {
    class SubSubForm extends Form {
      val field1 = new StringField(None)
    }
    class SubForm extends Form {
      val field1 = new StringField(None)
      val sub = new SubSubForm
    }
    class TestForm extends Form {
      val sub: SubForm = new SubForm
      val field1: StringField = new StringField(None)
    }
    val form = new TestForm
    assertTrue(form.field1.value.isEmpty)
    assertTrue(form.sub.field1.value.isEmpty)
    assertTrue(form.sub.sub.field1.value.isEmpty)

    form.merge(Array(
      new ChangeSet {
        field = "field1"
        value = "Kaare"
      },
      new ChangeSet {
        field = "sub.field1"
        value = "Funker dette"
      },
      new ChangeSet {
        field = "sub.sub.field1"
        value = "Jeez"
      }))

    assertEquals(Some("Kaare"), form.field1.value)
    assertEquals(Some("Funker dette"), form.sub.field1.value)
    assertEquals(Some("Jeez"), form.sub.sub.field1.value)
  }

  @Test
  def shouldAllowSelectFields() {
    class TestForm extends Form {
      val field1 = new SelectField(None)
      field1.options = List(SelectItem("ford", "Ford"), SelectItem("drittbil", "Audi"))
    }

    val form = new TestForm
    assertEquals(3, form.field1.getOptions.size)
    form.field1.value = Some("drittbil")
    assertEquals(Some("drittbil"), form.field1.value)
    assertEquals("Audi", form.field1.getDisplayText)
  }

  @Test
  def fieldNamesShouldMatch() {
    class TestForm extends Form {
      val field1 = new StringField("Something")
      val field2 = new StringField("Something else")
      val field3 = new SelectField(None)
      val field4 = new SelectField(None)
      val field5 = new RadioField(None)
      val field6 = new RadioField(None)
      val field7 = new IntField(None)
      val field8 = new IntField(None)
      val field9 = new DisplayTextField(None)
      val field10 = new DisplayTextField(None)

    }

    val form = new TestForm
    assertEquals("field1", form.field1.name)
    assertEquals("field2", form.field2.getName)
    assertEquals("field3", form.field3.name)
    assertEquals("field4", form.field4.name)
    assertEquals("field5", form.field5.name)
    assertEquals("field6", form.field6.name)
    assertEquals("field7", form.field7.name)
    assertEquals("field8", form.field8.name)
    assertEquals("field9", form.field9.name)
    assertEquals("field10", form.field10.name)
  }

  @Test
  def fieldNamesShouldMatchInSubForms() {
    class SubForm extends Form {
      val field3 = new StringField("Something completly different")
    }
    class TestForm extends Form {
      val field1 = new StringField("Something")
      val field2 = new StringField("Something else")
      val sub = new SubForm
    }

    val form = new TestForm
    assertEquals("field1", form.field1.name)
    assertEquals("field2", form.field2.getName)
    assertEquals("field3", form.sub.field3.name)
  }

  @Test
  def triggerChangeListenersInSubForms() {
    class SubSubForm extends Form {
      val field1 = new StringField("sub.sub.field1 value")
    }
    class SubForm extends Form {
      val field1 = new StringField("sub.field1 value")
      val sub = new SubSubForm

      field1.onChange = () => sub.field1.value = Some("sub.sub.field1 and now even did I")
    }
    class TestForm extends Form {
      val sub: SubForm = new SubForm
      val field1: StringField = new StringField("field1 value")

      field1.onChange = () => sub.field1.value = Some("sub.field1 changed as well")
    }

    val form = new TestForm

    assertEquals(Some("field1 value"), form.field1.value)
    assertEquals(Some("sub.field1 value"), form.sub.field1.value)
    assertEquals(Some("sub.sub.field1 value"), form.sub.sub.field1.value)


    form.merge(Array(
      new ChangeSet {
        field = "field1";
        value = "field1 changed"
      }
    ))

    assertEquals(Some("field1 changed"), form.field1.value)
    assertEquals(Some("sub.field1 changed as well"), form.sub.field1.value)
    assertEquals(Some("sub.sub.field1 value"), form.sub.sub.field1.value)

    form.merge(Array(
      new ChangeSet {
        field = "sub.field1";
        value = "sub.field1 changed"
      }
    ))

    assertEquals(Some("field1 changed"), form.field1.value)
    assertEquals(Some("sub.field1 changed"), form.sub.field1.value)
    assertEquals(Some("sub.sub.field1 and now even did I"), form.sub.sub.field1.value)
  }

  @Test
  def triggerChangeListeners() {
    class TestForm extends Form {
      val field1 = new StringField("Something")
      val field2 = new StringField("Something else")

      field1.onChange = () => {
        field2.value = field1.value
      }
    }
    val form = new TestForm

    assertEquals(Some("Something"), form.field1.value)
    assertEquals(Some("Something else"), form.field2.value)

    val changeSet: Array[ChangeSet] = Array(new ChangeSet {
      field = "field1";
      value = "Wee I have changed too"
    })

    form.merge(changeSet)
    assertEquals(Some("Wee I have changed too"), form.field1.value)
    assertEquals(Some("Wee I have changed too"), form.field2.value)
  }

}
