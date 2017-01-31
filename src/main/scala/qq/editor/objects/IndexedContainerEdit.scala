package qq.editor.objects

import de.ust.skill.common.scala.api;

class IndexedContainerEdit[C <: scala.collection.mutable.ArrayBuffer[_], O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  private var firstIndex = 0
  private val pageSize = 10 // TODO preferences


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
        updateHeadValues(null)
      }
    }
  }
  private val pgUpAct = new swing.Action("Previous Page") {
    icon = new qq.icons.BackIcon(true, true)
     override def apply() {
      val n = obj.get(field).size
      if (firstIndex > 0) {
        firstIndex -= pageSize min firstIndex
        updateHeadValues(null)
      }
    }
  }
  
  private val pgUpBtn = new qq.util.PlainButton(pgUpAct) {
    text = ""
  }
  private val pgDnBtn = new qq.util.PlainButton(pgDnAct) {
    text = ""
  }


  private val updateHeadValues: (qq.editor.Edit[_] ⇒ Unit) = { _ ⇒
    val n = obj.get(field).size
    countLbl.text = "" + n + " elements"
    shownLbl.text = "" + firstIndex + " to " + ((firstIndex + pageSize - 1) min (n - 1)) + " of " + n
    pgUpAct.enabled = firstIndex > 0
    pgDnAct.enabled = firstIndex + pageSize < n
  }

  page.file.onEdit.weak += updateHeadValues

  private val head = qq.util.Swing.HBox()
  
  private def switchHeadStyle(expanded: Boolean) = {
    head.contents.clear()
    head.contents ++= Seq(nameLbl, swing.Swing.HGlue)
    if (expanded) {
      head.contents ++= Seq(pgUpBtn, shownLbl, pgDnBtn)
    } else {
      head.contents += countLbl
    }
  }
  private val en = new qq.util.ExpandableNode(head) {
    override def onCollapse() = {switchHeadStyle(false)}
    override def onExpand() = {switchHeadStyle(true)}
  }
 
  en.subPart = new swing.Label("dummy")
  contents += en
  
  updateHeadValues(null)
  en.collapse()  
  
}