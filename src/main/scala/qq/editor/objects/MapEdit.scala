package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes.MapType
import de.ust.skill.common.scala.internal.fieldTypes.FieldType
import scala.collection.mutable.HashMap;
import qq.util.Swing.HBox
import qq.util.Swing.VBox
import swing.Swing.HGlue

class MapEdit[K, V, C[K, V] <: HashMap[K, V], O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[K, V]])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val skillType = field.t.asInstanceOf[MapType[K, V]]
  val groundTypes = MapEdit.typeList(skillType)
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
      val n = MapEdit.size(obj.get(field), skillType)
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
      val n = MapEdit.size(obj.get(field), skillType)
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
    val n = MapEdit.size(obj.get(field), skillType)
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

  private val head = qq.util.Swing.HBox()

  private def switchHeadStyle(expanded: Boolean) = {
    head.contents.clear()
    head.contents ++= Seq(nameLbl, HGlue)
    if (expanded) {
      head.contents ++= Seq(pgUpBtn, shownLbl, pgDnBtn)
    } else {
      head.contents += countLbl
    }
  }
  private val en = new qq.util.ExpandableNode(head) {
    override def onCollapse() = { switchHeadStyle(false) }
    override def onExpand() = { switchHeadStyle(true) }
  }

  private val lowerPart = new swing.BoxPanel(swing.Orientation.Vertical)
  private def refillLower(): Unit = {
    lowerPart.contents.clear()
    lowerPart.contents ++= MapEdit.keys(obj.get(field), skillType).toSeq.sortBy(x ⇒ if (x == null) "" else x.toString).drop(firstIndex).take(pageSize).map { key ⇒
      val keysbox = VBox()
      for ((k, t) ← key.zip(groundTypes)) {
        keysbox.contents += qq.util.Swing.HBox(new GroundValueLabel(page, t, k), swing.Swing.HGlue)
      }
      val fprop = qq.editor.binding.MapField(null, page.file, pool, obj, field, key, elType)
      val valed = new ElementFieldEdit(
        page,
        elType,
        fprop,
        false)
      val ra = new swing.Action("remove") {
        icon = new qq.icons.RemoveListItemIcon(true)
        override def apply() {
          new qq.editor.UserMapRemove[O, C[K,V]](page.file, pool, obj, field, key)
        }
      }
      HBox(0.0f,
        VBox(0.0f,
          HBox(0.0f, //new swing.Label("⋅"),
            keysbox),
          HBox(0.0f, //new swing.Label("↦"),
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
        val keys = for(τ <- groundTypes.dropRight(1)) yield NewValue.prompt(τ, "key", page)
        new qq.editor.UserMapInsert(page.file, pool, obj, field, keys, NewValue.default(groundTypes.last))
      }
    }
    lowerPart.contents += qq.util.Swing.HBox(0.0f,
      new swing.Label(if (firstIndex + pageSize >= MapEdit.size(obj.get(field), skillType)) s"end of ${field.name}" else ""),
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

object MapEdit {
  /** flatten the nested map type into a list of ground types */
  def typeList(τ: MapType[_, _]): Seq[FieldType[_]] = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        τ.keyType +: typeList(τ2)
      case _ ⇒
        Seq(τ.keyType, τ.valueType)
    }
  }
  /** number of entries in a map (complete ones) */
  def size(m: HashMap[_, _], τ: MapType[_, _]): Int = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        m.map(e ⇒ MapEdit.size(e._2.asInstanceOf[HashMap[_, _]], τ2)).sum
      case _ ⇒
        m.size
    }
  }
  /** enumerate all key tuples */
  def keys(m: HashMap[_, _], τ: MapType[_, _]): Iterable[Seq[Any]] = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        m.flatMap(e ⇒ keys(e._2.asInstanceOf[HashMap[_, _]], τ2).map(f ⇒ e._1 +: f))
      case _ ⇒
        m.keys.map(Seq(_))
    }
  }

  def get(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any]): Any = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        get(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail)
      case _ ⇒
        m(key.head)
    }
  }
  def contains(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any]): Boolean = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        m.contains(key.head) && contains(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail)
      case _ ⇒
        m.contains(key.head)
    }
  }
  def set(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any], value: Any): Unit = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        set(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail, value)
      case _ ⇒
        m(key.head) = value
    }
  }
  def insert(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any], value: Any): Unit = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        if (!m.contains(key.head)) {
          m(key.head) = new HashMap()
        }
        insert(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail, value)
      case _ ⇒
        m(key.head) = value
    }
  }
  def remove(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any]): Unit = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        remove(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail)
        if (m(key.head).asInstanceOf[HashMap[Any, Any]].size == 0) {
          m.remove(key.head) 
        }
      case _ ⇒
        m.remove(key.head)
    }
  }


}