package qq.util

/**
 * Plain label style for document content, just text
 */
class PlainLabel(title0: String) extends swing.Label(title0) {
  this.foreground = java.awt.SystemColor.textText
  this.background = java.awt.SystemColor.text
  this.font = new java.awt.Font("SansSerif", java.awt.Font.PLAIN, this.font.getSize)
}