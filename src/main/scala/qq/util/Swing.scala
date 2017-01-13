package qq.util

object Swing {
  def HBox(xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Horizontal) {
    contents ++= xs
  }
  def VBox(xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Vertical) {
    contents ++= xs
  }
}
