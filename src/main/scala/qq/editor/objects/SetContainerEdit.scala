package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.FieldDeclaration;
import de.ust.skill.common.scala.internal.fieldTypes.SingleBaseTypeContainer;
import de.ust.skill.common.scala.internal.fieldTypes.FieldType;
import scala.collection.mutable.HashSet;

/** Swing Ui element for modifying the contents of a set field. */

class SetContainerEdit[E, C[E] <: HashSet[E], O <: api.SkillObject](
  val page: qq.editor.Page,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[E]])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  private val groundType = field.t.asInstanceOf[SingleBaseTypeContainer[_, _]].groundType.asInstanceOf[FieldType[E]]
  
  private var firstIndex = 0
  private val pageSize = qq.editor.Main.preferences.editCollectionPageSize()

  /** label showing field name */
  private val nameLbl = new swing.Label(field.name)

  /** label showing number of elements when collapsed*/
  private val countLbl = new swing.Label("-")
  /** label showing indices of visible elements when expanded */
  private val shownLbl = new swing.Label("-")
  /** next page action */
  private val pgDnAct = new swing.Action("Next Page") {
    icon = new qq.icons.ForwardIcon(true, true)
    override def apply() {
      val n = obj.get(field).size
      if (firstIndex + pageSize < n) {
        firstIndex += pageSize
        updateHeadValues
        refillLower
      }
    }
  }
  private val pgUpAct = new swing.Action("Previous Page") {
    icon = new qq.icons.BackIcon(true, true)
    override def apply() {
      val n = obj.get(field).size
      if (firstIndex > 0) {
        firstIndex -= pageSize min firstIndex
        updateHeadValues
        refillLower
      }
    }
  }

  private val pgUpBtn = new qq.util.PlainButton(pgUpAct) {
    text = ""
    this.disabledIcon = new qq.icons.BackIcon(false, true)
  }
  private val pgDnBtn = new qq.util.PlainButton(pgDnAct) {
    text = ""
    this.disabledIcon = new qq.icons.ForwardIcon(false, true)
  }

  /** update number of elements and current position in header */
  private def updateHeadValues(): Unit = {
    val n = obj.get(field).size
    countLbl.text = "" + n + " elements"
    shownLbl.text = "" + firstIndex + " to " + ((firstIndex + pageSize - 1) min (n - 1)) + " of " + n
    pgUpAct.enabled = firstIndex > 0
    pgDnAct.enabled = firstIndex + pageSize < n /* when length is variable there is also a last line for appending, but we will make the page too long when necessary instead of moving it to a separate page */
  }

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { e ⇒
    if (e.obj == obj) {
      e match {
        case e: qq.editor.SetEdit[E, C[E], O] ⇒
          if (e.field == field) {
            e match {
              case e: qq.editor.SetInsert[E, C[E], O] ⇒
                updateHeadValues
                refillLower
              case e: qq.editor.SetRemove[E, C[E], O] ⇒
                updateHeadValues
                refillLower
              case _ ⇒ ()
            }
          }
        case _ ⇒ ()
      }
    }
  }

  page.file.onEdit.weak += fileEditHandler

  private val head = qq.util.Swing.HBoxD()

  private def switchHeadStyle(expanded: Boolean) = {
    head.contents.clear()
    head.contents ++= Seq(nameLbl, swing.Swing.HGlue)
    if (expanded) {
      head.contents ++= Seq(pgUpBtn, shownLbl, pgDnBtn)
    } else {
      head.contents += countLbl
    }
    head.contents.foreach(_.tooltip = s"${field.t} ${field.name}")
  }
  private val en = new qq.util.ExpandableNode(head, false) {
    override def onCollapse() = { switchHeadStyle(false) }
    override def onExpand() = { switchHeadStyle(true) }
  }

  private val lowerPart = new swing.BoxPanel(swing.Orientation.Vertical)
  private def refillLower(): Unit = {
    lowerPart.contents.clear()
    lowerPart.contents ++= obj.get(field).toSeq.sortBy(x ⇒ if (x == null) "" else x.toString).drop(firstIndex).take(pageSize).map { key ⇒
      val fprop = new qq.editor.binding.SetContainerField(null, page.file, pool, obj, field, key)
      val fed = new ElementFieldEdit(
        page,
        groundType,
        fprop)
      val ra = new swing.Action("remove") {
        icon = new qq.icons.RemoveListItemIcon(true)
        override def apply() {
          new qq.editor.UserSetRemove[O, C[E], E](page.file, pool, obj, field, fprop())
        }
      }
      qq.util.Swing.HBoxD(0.0f,
        fed,
        new qq.util.PlainButton(ra) { text = "" })
    }
    /* add a row for inserting at the end; if the fields end at a page break,
       * the last page will be too long (due to this append line), but that's,
       * I think, less bad then having the append-line on its own page */
    val aa = new swing.Action("add") {
      icon = new qq.icons.AddListItemIcon(true)
      override def apply() {
        NewValue.promptInPage(groundType, s"New element for set ${field.name}",
            page, field.asInstanceOf[FieldDeclaration[_,_]].restrictions,
            (x : E) => { 
              new qq.editor.UserSetInsert(page.file, pool, obj, field, x)
              page.tabbedPane.addPage(page)
            },
            _ =>  page.tabbedPane.addPage(page))
          page.tabbedPane.removePage(page.index)
      }
    }
    lowerPart.contents += qq.util.Swing.HBoxD(0.0f,
      new swing.Label(if (firstIndex + pageSize >= obj.get(field).size) s"end of ${field.name}" else ""),
      swing.Swing.HGlue,
      new qq.util.PlainButton(aa) { text = "" })

  }
  en.subPart = lowerPart
  contents += en

  updateHeadValues
  refillLower
  if (obj.get(field).size > 0 && obj.get(field).size <= qq.editor.Main.preferences.editCollectionSmall()) {
    en.expand()
  } else {
    en.collapse()
  }

}