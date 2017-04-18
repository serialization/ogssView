package qq.editor

import scala.swing;

/** A page that displays something from file; properties are the editor settings. */
abstract class Page(val file: File,
     val settings: EditorPreferences) extends qq.util.TabbedPane.ClosablePage("", new swing.Label("")) {
  /** menu items that this page adds to the view menu */
  def viewMenuItems: Seq[swing.MenuItem]
  /** menu items that this page adds to the type menu */
  def typeMenuItems: Seq[swing.MenuItem]
   /** menu items that this page adds to the object menu */
  def objectMenuItems: Seq[swing.MenuItem]
   
}