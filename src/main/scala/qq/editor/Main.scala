package qq.editor

import swing._;
import event._;

object Main extends SimpleSwingApplication {

  /** the current file */
  var file: File = null

  /** event which is fired whenever file is changes (parameter is new value of file) */
  val onFileChange: qq.util.binding.Event[File] = new qq.util.binding.Event()

  val tabs = new qq.util.TabbedPane()

  def closeFile: Unit = {
    if (file != null) {
      if (file.isModified) {
        val x = Dialog.showConfirmation(null,
          "Discard unsaved changes in " + file.fileNameOnly,
          "Discard Unsaved Changes",
          Dialog.Options.YesNo,
          Dialog.Message.Question, null)

        if (x != Dialog.Result.Yes) throw new Exception()
      }
      while (tabs.pages.length > 0) tabs.removePage(0)
      file = null
      onFileChange.fire(file)
    }
  }

  val actOpen = new Action("Open") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl O"))
    mnemonic = swing.event.Key.O.id
    override def apply() {
      closeFile
      val fc = new FileChooser()
      val result = fc.showOpenDialog(null)
      if (result == FileChooser.Result.Approve) {
        file = new File(fc.selectedFile.toString())
        undoMenuItem.action = file.undoManager.undoAction
        redoMenuItem.action = file.undoManager.redoAction
        onFileChange.fire(file)
      }
    }
  }
  val actSave = new Action("Save") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl S"))
    mnemonic = swing.event.Key.S.id
    def setEnabled: Unit = {
      enabled = file != null && file.isModified
    }
    onFileChange.strong += (_ ⇒ setEnabled)
    onFileChange.strong += (file ⇒ if (file != null) file.onEdit.strong += (_ ⇒ setEnabled))
    override def apply() {
      /* TODO save file */
    }
  }
  val actSaveAs = new Action("Save As …") {
    mnemonic = swing.event.Key.A.id
    onFileChange.strong += (file ⇒ enabled = file != null)
    override def apply() {
      /* TODO save file */
    }
  }
  val actClose = new Action("Close") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl C"))
    mnemonic = swing.event.Key.C.id
    onFileChange.strong += (file ⇒ enabled = file != null)
    override def apply() {
      closeFile
    }
  }
  val actUndoDummy = new Action("nothing to undo") {
    enabled = false
    override def apply() {}
  }
  val actRedoDummy = new Action("nothing to redo") {
    enabled = false
    override def apply() {}
  }
  val undoMenuItem = new MenuItem("") {
    onFileChange.strong += (file ⇒
      action = if (file != null)
        file.undoManager.undoAction
      else actUndoDummy)
  }
  val redoMenuItem = new MenuItem("") {
    onFileChange.strong += (file ⇒
      action = if (file != null)
        file.undoManager.redoAction
      else actRedoDummy)
  }
  val viewMenu = new Menu("View") {
    mnemonic = swing.event.Key.V
    onFileChange.strong += (_ ⇒ enabled = false) // enable when something is shown
    tabs.onPageChanged.strong += { page ⇒
      contents.clear()
      if (page != null && page.isInstanceOf[qq.editor.Page]) {
        enabled = true
        contents ++= page.asInstanceOf[qq.editor.Page].viewMenuItems
      } else {
        enabled = false
      }
    }
  }
  val newObjectPageAction = new Action("New Object Page") {
    mnemonic = swing.event.Key.N.id
    override def apply() = {
    }
  }
  val newObjectPageMenuItem = new qq.util.TodoMenuItem("New Object Page") //newObjectPageAction)
  val objectMenu = new Menu("Object") {
    mnemonic = swing.event.Key.O
    onFileChange.strong += { file ⇒
      contents.clear()
      enabled = file != null
      if (enabled) contents += newObjectPageMenuItem
    }
    tabs.onPageChanged.strong += { page ⇒
      contents.clear()
      contents += newObjectPageMenuItem
      if (page != null && page.isInstanceOf[qq.editor.Page]) {
        val pageItems = page.asInstanceOf[qq.editor.Page].objectMenuItems
        if (pageItems.length > 0) {
          contents += new swing.Separator
          contents ++= pageItems
        }
      }
    }
  }
  val newTypePageAction = new Action("New Type Page") {
    mnemonic = swing.event.Key.N.id
    override def apply() = {
      tabs.addPage(new qq.editor.types.TypePage(file))
    }
  }
  val newTypePageMenuItem = new MenuItem(newTypePageAction)
  val typeMenu = new Menu("Type") {
    mnemonic = swing.event.Key.T
    onFileChange.strong += { file ⇒
      contents.clear()
      enabled = file != null
      if (enabled) contents += newTypePageMenuItem
    }
    tabs.onPageChanged.strong += { page ⇒
      contents.clear()
      contents += newTypePageMenuItem
      contents += new qq.util.TodoMenuItem("Profiles")
      if (page != null && page.isInstanceOf[qq.editor.Page]) {
        val pageItems = page.asInstanceOf[qq.editor.Page].typeMenuItems
        if (pageItems.length > 0) {
          contents += new swing.Separator
          contents ++= pageItems
        }
      }
    }
  }
  val menu = new MenuBar {
    contents ++= Seq(
      new Menu("File") {
        mnemonic = swing.event.Key.F
        contents ++= Seq(
          new MenuItem(actOpen),
          new MenuItem(actSave),
          new MenuItem(actSaveAs),
          new MenuItem(actClose))
      },
      new Menu("Edit") {
        mnemonic = swing.event.Key.E
        contents ++= Seq(undoMenuItem, redoMenuItem)
      },
      viewMenu, objectMenu, typeMenu)
  }
  def top = new MainFrame {
    menuBar = menu
    contents = tabs

    onFileChange.strong += (file ⇒
      title = if (file != null)
        file.windowTitle
      else
        "SKilL Editor")

    onFileChange.fire(file)
  }

  // TODO delete
  file = new File("C:\\Users\\m\\stud\\dt\\testinp\\time.iml.sf")
  undoMenuItem.action = file.undoManager.undoAction
  redoMenuItem.action = file.undoManager.redoAction
  onFileChange.fire(file)

}
