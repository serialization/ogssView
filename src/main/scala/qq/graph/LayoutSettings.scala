package qq.graph

import qq.util.binding.PropertyOwner
import qq.util.binding.Property

/** properties that control the layout algorithm */
class LayoutSettings
extends PropertyOwner {
  val c1: Property[Float] = new Property(this, "edge force scale factor (c1)", 2.0f)
  val c3: Property[Float] = new Property(this, "node-node repulsion force scale factor (c3)", 10000.0f)
  val c2: Property[Float] = new Property(this, "desired edge length in pixels (c2)", 100.0f)
  val ε: Property[Float] =  new Property(this, "minimum edge length in pixels used for calculation when nodes overlap (ε)", 5f)
  
}