package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

/**
 * A control for displaying a type on a page with user types turned into links
 */
class TypeNameControl(val page: qq.editor.types.TypePage,
    val τ: api.Access[_]) extends swing.BoxPanel(swing.Orientation.Horizontal) {
  
  contents += new qq.util.PlainButton(swing.Action(τ.name) {
         page.goTo(τ)
      }) {
  //      this.peer.setComponentPopupMenu(new swing.PopupMenu() {contents ++= TypeMenu(τ, subgraph)}.peer)
      }
  
}