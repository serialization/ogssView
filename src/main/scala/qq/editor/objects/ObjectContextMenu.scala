package qq.editor.objects

import de.ust.skill.common.scala.api

object ObjectContextMenu {
  def apply(o: api.SkillObject, page: ObjectPage): swing.PopupMenu = {
    new swing.PopupMenu() {
      contents ++= Seq(
        new swing.MenuItem(swing.Action("Go to") {
          page.goTo(o)
        })    ,
        new swing.MenuItem(swing.Action("Open in new page") {
          qq.editor.Main.newObjectTab(o)
        })    
      )
    }
  }
}