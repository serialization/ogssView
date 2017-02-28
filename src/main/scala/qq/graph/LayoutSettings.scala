package qq.graph

import qq.util.binding.PropertyOwner
import qq.util.binding.Property

/** properties that control the layout algorithm */
class LayoutSettings
extends PropertyOwner {
  val c1: Property[Float] = new Property(this, "Edge force scale factor (c1)", 2.0f)
  val c3: Property[Float] = new Property(this, "Node-node repulsion force scale factor (c3)", 8000.0f)
  val c2: Property[Float] = new Property(this, "Desired edge length in pixels (c2)", 100.0f)
  val c4: Property[Float] = new Property(this, "Edge direction force scale factor (c4)", 2.0f)
  val c5: Property[Float] = new Property(this, "Edge direction force scale factor (c5)", 0.7f)
  val ε: Property[Float] =  new Property(this, "Minimum edge length in pixels used for calculation when nodes overlap (ε)", 5f)
  val cluttered: Property[Float] = new Property(this, "`Energy' threshold per node for cluttered layouts", 3.0f)
  val rootAtCentre: Property[Boolean] = new Property(this, "Fix position of root node to centre", true) 
  val rootBold: Property[Boolean] = new Property(this, "Use bold border for root", true) 
  val initialIterations: Property[Int] = new Property(this, "Iterations without overlap avoidance", 50)
  val phaseInIterations: Property[Int] = new Property(this, "Iterations during which overlap avoidance is phased in", 50)
  val finalIterations: Property[Int] = new Property(this, "Iterations with overlap avoidance fully active", 50)
  def iterations = initialIterations() + phaseInIterations() + finalIterations()
}