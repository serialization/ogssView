package qq.editor

sealed abstract class FieldVisibility {
  val showAsNode: Boolean
  val showInParent: Boolean
}

case object HideVisibility extends FieldVisibility {
  override val showAsNode: Boolean = false
  override val showInParent: Boolean = false
}

case object ShowAsNodeVisibility extends FieldVisibility {
  override val showAsNode: Boolean = true
  override val showInParent: Boolean = false
}

case object ShowInParentVisibility extends FieldVisibility {
  override val showAsNode: Boolean = false
  override val showInParent: Boolean = true
}