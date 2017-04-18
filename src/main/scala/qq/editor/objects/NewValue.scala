package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;
import de.ust.skill.common.scala.internal.restrictions._;
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.PromptSkillFieldProperty
import qq.editor.Page
import swing.Action
import swing.Label
import swing.Button
import swing.Swing.HGlue
import swing.Swing.VGlue
import qq.util.Swing.VBoxD
import qq.util.Swing.HBoxD

/**
 * Functions to create a new value of a ground type; either default or ask the user
 */
object NewValue {
  def default[T](τ: api.FieldType[T], restrictions: HashSet[FieldRestriction]): T = {
    // default?
    restrictions.foreach {
      case r: DefaultRestriction[T] ⇒ return r.value;
      case _                        ⇒ ()
    }
    // range?
    restrictions.foreach {
      case Range.RangeI8(min, max)  ⇒ if (0 < min) return min.asInstanceOf[T];
      case Range.RangeI16(min, max) ⇒ if (0 < min) return min.asInstanceOf[T];
      case Range.RangeI32(min, max) ⇒ if (0 < min) return min.asInstanceOf[T];
      case Range.RangeI64(min, max) ⇒ if (0L < min) return min.asInstanceOf[T];
      case Range.RangeF32(min, max) ⇒ if (0L < min) return min.asInstanceOf[T];
      case Range.RangeF64(min, max) ⇒ if (0L < min) return min.asInstanceOf[T];
      case _                        ⇒ ()
    }

    τ.asInstanceOf[FieldType[T]] match {
      case BoolType  ⇒ false.asInstanceOf[T]
      case I8        ⇒ 0.toByte.asInstanceOf[T]
      case I16       ⇒ 0.toShort.asInstanceOf[T]
      case I32       ⇒ 0.asInstanceOf[T]
      case I64 | V64 ⇒ 0L.asInstanceOf[T]
      case F64       ⇒ 0.0.asInstanceOf[T]
      case F32       ⇒ 0.0f.asInstanceOf[T]
      case _: AnnotationType | _: UserType[_] ⇒
        null.asInstanceOf[T]
      case _: StringType ⇒ "".asInstanceOf[T]
      case τ: ConstantLengthArray[e] ⇒
        val x = new ArrayBuffer[e](τ.length)
        for (_ ← 0 until τ.length) {
          x += default(τ.groundType, restrictions)
        }
        x.asInstanceOf[T]
      case _: ListType[e] ⇒
        (new ListBuffer[e]()).asInstanceOf[T]
      case _: VariableLengthArray[e] ⇒
        (new ArrayBuffer[e]()).asInstanceOf[T]
      case _: SetType[e] ⇒
        (new HashSet[e]()).asInstanceOf[T]
      case _: MapType[k, v] ⇒
        (new HashMap[k, v]()).asInstanceOf[T]
      case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
        | ConstantV64(_) ⇒
        throw new Exception(s"constant field type $τ does not have a default value")
    }
  }

  class PromptPage[T](file0: qq.editor.File,
                      settings0: qq.editor.EditorPreferences,
                      val prompt: String,
                      val τ: api.FieldType[T],
                      val restrictions: HashSet[FieldRestriction],
                      val onSelect: T ⇒ Unit,
                      val onCancel: Unit ⇒ Unit)
      extends qq.editor.Page(file0, settings0) {

    def viewMenuItems = Seq()
    def typeMenuItems = Seq()
    def objectMenuItems = Seq()

    val objSelectErrorLabel = new swing.Label("-") { foreground = java.awt.Color.red; visible = false }

    val p = new PromptSkillFieldProperty(null, prompt, default(τ, restrictions), τ)
    val ed = new ElementFieldEdit(this, τ.asInstanceOf[FieldType[T]], p)

    val accept = Action("Ok") {
      try {
        ed.editField.componentToProperty()
        onSelect(p())
        tabbedPane.removePage(index)
      } catch {
        case e: Exception ⇒
          objSelectErrorLabel.text = qq.util.binding.ValidationExceptionMessage(e)
          objSelectErrorLabel.visible = true
      }
    }
    val cancel = Action("Cancel") {
      try {
        onCancel(())
        tabbedPane.removePage(index)
      } catch {
        case e: Exception ⇒
          objSelectErrorLabel.text = qq.util.binding.ValidationExceptionMessage(e)
          objSelectErrorLabel.visible = true
      }
    }

    title = "New Value"
    content = VBoxD(
      HBoxD(HGlue, new Label(s"<html><h1>$title</h1></html>"), HGlue),
      VGlue,
      HBoxD(HGlue, ed, HGlue),
      VGlue,
      HBoxD(HGlue, objSelectErrorLabel, HGlue, new Button(accept), new Button(cancel)))
  }

  def promptInPage[T](τ: api.FieldType[T],
                      prompt: String,
                      page: Page,
                      restrictions: HashSet[FieldRestriction],
                      onSelect: T ⇒ Unit,
                      onCancel: Unit ⇒ Unit): Unit = {

    τ.asInstanceOf[FieldType[T]] match {
      case _: AnnotationType | _: UserType[_] ⇒
        val sel = τ match {
          case u: api.Access[_] ⇒ qq.editor.Main.newObjectTab(u)
          case _                ⇒ qq.editor.Main.newObjectTab()
        }
        sel.select(prompt, onSelect, _ ⇒ onCancel(()))
      case _ ⇒
        val sel = new PromptPage(page.file, page.settings, prompt, τ, restrictions, onSelect, onCancel)
        page.tabbedPane.addPage(sel)
        sel.show()
    }

  }

}