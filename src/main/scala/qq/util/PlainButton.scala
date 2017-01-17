package qq.util

class PlainButton() extends swing.Button() {
  def this(a: swing.Action) = {this; action = a}
  this.border = swing.Swing.EmptyBorder(0)
  this.contentAreaFilled = false
  this.foreground = java.awt.Color.BLUE
}
