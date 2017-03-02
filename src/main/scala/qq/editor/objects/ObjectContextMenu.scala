package qq.editor.objects

import de.ust.skill.common.scala.api

object ObjectContextMenu {
  def apply(o: api.SkillObject, page: ObjectPage): swing.PopupMenu = {
    new swing.PopupMenu() {
      contents ++= Seq(
        new swing.MenuItem(swing.Action("Go to") {
          page.goTo(o)
        }),
        new swing.MenuItem(swing.Action("Open in new page") {
          qq.editor.Main.newObjectTab(o)
        }),
        new swing.MenuItem(swing.Action("Show/hide fields") {
          val τ = page.file.s(o.getTypeName)
          val frame = new swing.Frame() {
            title = s"Field preferences of ${τ.name}"
            contents = new qq.editor.types.TypeEdit(null, page.file, τ)
          }
          frame.visible = true
        }) 
      )
    }
  }
}