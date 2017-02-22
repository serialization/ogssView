package qq.editor

import qq.util.binding.Property
import java.util.prefs.Preferences

class Settings {

  private val prefs = Preferences.userRoot().node(s"/qq/skilledit");

  val graphLayout = new qq.graph.LayoutSettings()
  /** Page size of collections in the edit view */
  val editCollectionPageSize = new Property(null,
    "Members per page in collection editor",
    prefs.getInt("editCollectionPageSize", 10))
  editCollectionPageSize.onChange.strong += (prefs.putInt("editCollectionPageSize", _))

  val editCollectionSmall = new Property(null,
    "Maximum size of collections shown expanded in the editor",
    prefs.getInt("editCollectionSmall", 10))
  editCollectionSmall.onChange.strong += (prefs.putInt("editCollectionSmall", _))

  val graphCollectionSmall = new Property(null,
    "Maximum size of collections that get their members added to the graph",
    prefs.getInt("graphCollectionSmall", 5))
  graphCollectionSmall.onChange.strong += (prefs.putInt("graphCollectionSmall", _))
}  