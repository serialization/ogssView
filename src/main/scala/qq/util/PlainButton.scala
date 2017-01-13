package qq.util

class PlainButton() extends swing.Button() {
  def this(a: swing.Action) = {this; action = a}
  this.border = swing.Swing.EmptyBorder(1)
  this.contentAreaFilled = false
  this.foreground = java.awt.Color.BLUE
}
