package qq.editor.objects

import qq.editor.binding.SkillFieldProperty
import de.ust.skill.common.scala.api

class ReferenceEdit(val p: SkillFieldProperty[api.SkillObject], val page: qq.editor.Page, val addLabel: Boolean = true)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val editField = new qq.util.binding.TextEdit(p,
    page.file.objOfId(_),
    (x: api.SkillObject) ⇒ page.file.idOfObj(x))

  val labeledField = if (addLabel) new qq.util.binding.LabeledEdit(editField) else null

  val exn = new qq.util.ExpandableNode(if (addLabel) labeledField else editField, false)

  val mnuSelect = new swing.MenuItem(swing.Action("Select object") {
    val selection = p.groundType match {
      case u: api.Access[_] ⇒ qq.editor.Main.newObjectTab(u)
      case _                ⇒ qq.editor.Main.newObjectTab()
    }
    selection.select(s"Select new ${p.description}",
      { o ⇒
        p := o
        page.tabbedPane.addPage(page)
      },
      { o ⇒
        page.tabbedPane.addPage(page)
      })
    page.tabbedPane.removePage(page.index)
  })

  def onValueChange(x: api.SkillObject): Unit = {
    def setAllPopupMenus(x: swing.PopupMenu): Unit = {
      val peer = if (x == null) null else x.peer
      editField.tf.peer.setComponentPopupMenu(peer)
      if (addLabel) {
        labeledField.peer.setComponentPopupMenu(peer)
        labeledField.label.peer.setComponentPopupMenu(peer)
      }
    }
    if (x != null) {
      exn.lazySubPart = { x ⇒ new ObjectEdit(page, p()) }
      exn.collapse()

      val popupMenu = qq.editor.objects.ObjectContextMenu(x, page)
      popupMenu.contents += mnuSelect

      setAllPopupMenus(popupMenu)
    } else {
      exn.lazySubPart = null
      setAllPopupMenus(new swing.PopupMenu() { contents += mnuSelect })
    }
  }

  onValueChange(p())

  p.onChange.strong += onValueChange
  contents += exn
}