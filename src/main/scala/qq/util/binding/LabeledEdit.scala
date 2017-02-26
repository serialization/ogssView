package qq.util.binding

/**
 * Puts a label above an unlabled edit control.
 */
class LabeledEdit[T](val inner: EditControl[T])
    extends swing.Component with swing.Container.Wrapper with swing.SequentialContainer.Wrapper {

  private def leftColumnWidth = 25
  private def maxLeftColumnLabelWidth = leftColumnWidth - 2

  override lazy val peer = new javax.swing.JPanel with SuperMixin

  val label = new swing.Label(inner.property.name) { tooltip = inner.property.description }

  private val orientation =
    if (label.preferredSize.getWidth < maxLeftColumnLabelWidth)
      swing.Orientation.Horizontal
    else
      swing.Orientation.Vertical

  peer.setLayout(new javax.swing.BoxLayout(peer, orientation.id))

  if (orientation == swing.Orientation.Vertical) {
    contents += qq.util.Swing.HBox(
      label,
      swing.Swing.HGlue)
    contents += inner

  } else {
    contents += qq.util.Swing.VBox(
      qq.util.Swing.HBox(
        label,
        swing.Swing.HGlue),
      swing.Swing.RigidBox(new java.awt.Dimension(leftColumnWidth, 0)))
    contents += inner
  }

  override def enabled: Boolean = inner.enabled
  override def enabled_=(x: Boolean): Unit = inner.enabled_=(x)
}