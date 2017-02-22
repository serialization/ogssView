package qq.util.binding

import scala.collection.mutable;

/**
 * May be used for settings pages, I think. Mostly useless for skill field editors
 */
trait PropertyOwner {
  private def propertyOwner = this
  
  var properties: List[Property[_]] = Nil
  
  val onAnyPropertyChange: Event[Unit] = new Event
  def doOnAnyPropertyChange: Unit = onAnyPropertyChange.fire(())
  /**
   * for undoing edits made to this Property. It's a simple var instead of a list
   * of listeners because if I can add multiple undoEventListeners I have
   * to keep them in Synch somehow.
   * 
   * It's a member of the PropertyOwner because a) either all edits o a property
   * should be tracked or none, and b) properties belonging together will usuallt behave
   * alike; if they don't you still can put some of them into a different owner.
   */
  val undoManager: javax.swing.undo.UndoManager = null

  def propertyPage() = {
    val box = qq.util.Swing.VBox()
    box.contents ++= properties.map { x => new LabeledEdit(x.defaultEditor)}
    box
  }
}
