package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes.MapType
import de.ust.skill.common.scala.internal.fieldTypes.FieldType
import scala.collection.mutable.HashMap;
import qq.util.Swing.HBoxD
import qq.util.Swing.VBoxD
import swing.Swing.HGlue
import qq.util.FlattenedMap

class MapContainerEdit[K, V, C[K, V] <: HashMap[K, V], O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[K, V]])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val skillType = field.t.asInstanceOf[MapType[K, V]]
  val groundTypes = FlattenedMap.typeList(skillType)
  val elType = groundTypes.last

  private var firstIndex = 0
  private val pageSize = qq.editor.Main.settings.editCollectionPageSize()

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
      val n = FlattenedMap.size(obj.get(field), skillType)
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
      val n = FlattenedMap.size(obj.get(field), skillType)
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
    val n = FlattenedMap.size(obj.get(field), skillType)
    countLbl.text = "" + n + " elements"
    shownLbl.text = "" + firstIndex + " to " + ((firstIndex + pageSize - 1) min (n - 1)) + " of " + n
    pgUpAct.enabled = firstIndex > 0
    pgDnAct.enabled = firstIndex + pageSize < n /* when length is variable there is also a last line for appending, but we will make the page too long when necessary instead of moving it to a separate page */
  }

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { e ⇒
    if (e.obj == obj) {
            e match {
        case e: qq.editor.MapEdit[C[K, V], O] ⇒
          if (e.field == field) {
            e match {
              case _: qq.editor.MapInsert[C[K, V], O] | _: qq.editor.MapRemove[C[K, V], O]⇒
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

  private val head = HBoxD()

  private def switchHeadStyle(expanded: Boolean) = {
    head.contents.clear()
    head.contents ++= Seq(nameLbl, HGlue)
    if (expanded) {
      head.contents ++= Seq(pgUpBtn, shownLbl, pgDnBtn)
    } else {
      head.contents += countLbl
    }
  }
  private val en = new qq.util.ExpandableNode(head, false) {
    override def onCollapse() = { switchHeadStyle(false) }
    override def onExpand() = { switchHeadStyle(true) }
  }

  private val lowerPart = new swing.BoxPanel(swing.Orientation.Vertical)
  private def refillLower(): Unit = {
    lowerPart.contents.clear()
    lowerPart.contents ++= FlattenedMap.keys(obj.get(field), skillType).toSeq.sortBy(x ⇒ if (x == null) "" else x.toString).drop(firstIndex).take(pageSize).map { key ⇒
      val keysbox = VBoxD()
      for ((k, t) ← key.zip(groundTypes)) {
        keysbox.contents += qq.util.Swing.HBoxD(new GroundValueLabel(page, t, k), swing.Swing.HGlue)
      }
      val fprop = qq.editor.binding.MapContainerField(null, page.file, pool, obj, field, key, elType)
      val valed = new ElementFieldEdit(
        page,
        elType,
        fprop,
        false)
      val ra = new swing.Action("remove") {
        icon = new qq.icons.RemoveListItemIcon(true)
        override def apply() {
          new qq.editor.UserMapRemove[O, K, V, C](page.file, pool, obj, field, key)
        }
      }
      HBoxD(0.0f,
        VBoxD(0.0f,
          HBoxD(0.0f, //new swing.Label("⋅"),
            keysbox),
          HBoxD(0.0f, //new swing.Label("↦"),
            valed)),
        new qq.util.PlainButton(ra) { text = "" })
    }
    /* add a row for inserting at the end; if the fields end at a page break,
       * the last page will be too long (due to this append line), but that's,
       * I think, less bad then having the append-line on its own page */
    val aa = new swing.Action("add") {
      icon = new qq.icons.AddListItemIcon(true)
      override def apply() {
        // TODO nicer
        val keys = for(τ <- groundTypes.dropRight(1)) yield NewValue.prompt(τ, "key", page, scala.collection.mutable.HashSet())
        new qq.editor.UserMapInsert(page.file, pool, obj, field, keys, NewValue.default(groundTypes.last, scala.collection.mutable.HashSet()))
      }
    }
    lowerPart.contents += qq.util.Swing.HBoxD(0.0f,
      new swing.Label(if (firstIndex + pageSize >= FlattenedMap.size(obj.get(field), skillType)) s"end of ${field.name}" else ""),
      swing.Swing.HGlue,
      new qq.util.PlainButton(aa) { text = "" })

  }
  en.subPart = lowerPart
  contents += en

  updateHeadValues
  refillLower
  if (obj.get(field).size > 0 && obj.get(field).size <= qq.editor.Main.settings.editCollectionSmall()) {
    en.expand()
  } else {
    en.collapse()
  }

}

