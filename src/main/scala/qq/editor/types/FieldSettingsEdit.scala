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
  private def endis: Unit = {
    if (fs.prefHide()) fs.prefShow := false
    if (fs.prefShow()) fs.prefHide := false
    showNullEd.enabled = fs.prefShow()
    showInParentEd.enabled = fs.prefShow()
    fixDirEd.enabled = fs.prefShow() && !fs.prefShowInParent()
  }
  /* variants of endis that take the right parameters (needs to be
   * referenced from this to stay alive long enough) */
  private val endisb = (_: Boolean) => endis

  fs.prefHide.onChange.weak += endisb
  fs.prefShow.onChange.weak += endisb
  fs.prefShowNull.onChange.weak += endisb
  fs.prefShowInParent.onChange.weak += endisb
  fs.prefFixedEdgeDirection.onChange.weak += endisb

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
   endis
}