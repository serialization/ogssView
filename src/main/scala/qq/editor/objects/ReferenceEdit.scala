package qq.editor.objects

import qq.util.binding.Property
import de.ust.skill.common.scala.api

class ReferenceEdit(val p: Property[api.SkillObject], val page: ObjectPage, val addLabel: Boolean = true)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val editField = new qq.util.binding.TextEdit(p,
    page.file.objOfId(_),
    (x: api.SkillObject) ⇒ page.file.idOfObj(x))

  val labeledField = if (addLabel) new qq.util.binding.LabeledEdit(editField) else null

  val exn = new qq.util.ExpandableNode(if (addLabel) labeledField else editField, false)

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
      popupMenu.contents += new swing.MenuItem(swing.Action("Select object") {
          val selection = qq.editor.Main.newObjectTab()
          // TODO property needs to publish ground type
          selection.select(s"change ${p.name}",
              {o =>
                p := o           
                page.tabbedPane.addPage(page)
              },
              {o =>
                page.tabbedPane.addPage(page)
              })
          page.tabbedPane.removePage(page.index)
        })
      
      setAllPopupMenus(popupMenu)
    } else {
      exn.lazySubPart = null
      setAllPopupMenus(null)
    }
  }

  onValueChange(p())

  p.onChange.strong += onValueChange
  contents += exn
}