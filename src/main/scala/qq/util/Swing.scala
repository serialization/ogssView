package qq.util

object Swing {
  def HBox(xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Horizontal) {
    contents ++= xs
  }
  def HBox(vAlignment: Float, xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Horizontal) {
    xs.foreach { _.peer.setAlignmentY(vAlignment)}
    contents ++= xs
  }
  def VBox(xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Vertical) {
    contents ++= xs
  }
  def VBox(hAlignment: Float, xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Vertical) {
    xs.foreach { _.peer.setAlignmentX(hAlignment)}
    contents ++= xs
  }
}
