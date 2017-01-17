package qq.editor.types

import de.ust.skill.common.scala.api;
import qq.util.binding._;

/**
 * Edit the settings of field
 */
class FieldSettingsEdit(
  val file: qq.editor.File,
  val skillType: api.Access[_],
  val field: api.FieldDeclaration[_])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val fs = file.typeSettings(skillType).fields(field)

  val hideEd = new CheckBoxEdit(fs.prefHide)
  val showEd = new CheckBoxEdit(fs.prefShow)
  val showNullEd = new CheckBoxEdit(fs.prefShowNull)
  val showInParentEd = new CheckBoxEdit(fs.prefShowInParent)
  val fixDirEd = new CheckBoxEdit(fs.prefFixedEdgeDirection)

  /**
   * Enable and disable controls according to their value
   */
  def endis(x: Any): Unit = {
    if (fs.prefHide()) fs.prefShow := false
    if (fs.prefShow()) fs.prefHide := false
    showNullEd.enabled = fs.prefShow()
    showInParentEd.enabled = fs.prefShow()
    fixDirEd.enabled = fs.prefShow() && !fs.prefShowInParent()
  }

  fs.prefHide.onChange.weak += endis
  fs.prefShow.onChange.weak += endis
  fs.prefShowNull.onChange.weak += endis
  fs.prefShowInParent.onChange.weak += endis
  fs.prefFixedEdgeDirection.onChange.weak += endis

  contents ++= Seq(
      hideEd,
      showEd,
      qq.util.Swing.HBox(
        swing.Swing.RigidBox(new scala.swing.Dimension(15,0)),
        qq.util.Swing.VBox(
        showNullEd,
        showInParentEd,
        fixDirEd
        )
      ))
   endis(null)
}