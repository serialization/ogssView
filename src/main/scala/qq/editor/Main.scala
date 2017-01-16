package qq.editor

import swing._;
import event._;

object Main extends SimpleSwingApplication {

  /** the current file */
  var file: File = null

  /** event which is fired whenever file is changes (parameter is new value of file) */
  val onFileChange: qq.util.binding.Event[File] = new qq.util.binding.Event()

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
      })
  }

  def top = new MainFrame {
    menuBar = menu

    onFileChange.strong += (file ⇒
      title = if (file != null)
        file.windowTitle
      else
        "SKilL Editor")

    onFileChange.fire(file)
  }
}