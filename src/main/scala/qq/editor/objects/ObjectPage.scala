package qq.editor.objects

import de.ust.skill.common.scala.api;
import scala.collection.mutable;
import scala.swing;

/**
 * A page displaying data about a (set of) object, with an optional search results page on the
 * left and an option graph representation on the right
 */
class ObjectPage(file0: qq.editor.File, settings0: qq.editor.Settings) extends qq.editor.Page(file0, settings0) {

  /** search panel visible*/
  def searchVisible: Boolean = searchVisibleModel.isSelected()
  val searchVisibleModel = new javax.swing.JToggleButton.ToggleButtonModel()
  searchVisibleModel.setSelected(true)

  /** graph visible */
  def graphVisible: Boolean = graphVisibleModel.isSelected()
  val graphVisibleModel = new javax.swing.JToggleButton.ToggleButtonModel()

  class View(
      val obj: api.SkillObject,
      /** objects that are explicitly requested to be shown */
      val showAlso: mutable.HashSet[api.SkillObject] = new mutable.HashSet()) {
    def this(obj0: api.SkillObject, showAlso0: Iterable[api.SkillObject]) =
      this(obj0, new mutable.HashSet[api.SkillObject]() { this ++= showAlso0})
  }

  /** the object that is currently shown */
  var currentView: View = null

  /** previously shown types (for back navigation) */
  val previousView: mutable.Stack[View] = new mutable.Stack()

  /** previously previously shown types :) (for forward navigation) */
  val nextView: mutable.Stack[View] = new mutable.Stack()

  /**
   * a function that does something with an object. When not null, the page will be
   * in an object selection mode and contain a `select' button that will cause
   * tis continuation to be called with the current object
   */
  val objectSelectionContinuation: api.SkillObject ⇒ Unit = null
  /** Title for object selection */
  val objectSelectionTitle: String = ""

  /** show an object (internal, for goTo, goBack, goForward) */
  private def _goTo(v: View): Unit = {
    currentView = v
    title = file.idOfObj(v.obj)

    objEdit.contents.clear()
    objEdit.contents += new TopObjectEdit(this, v.obj)

    objGraph.contents.clear()
    objGraph.contents += new ObjectGraph(this, v.obj)
    
    goBack.enabled = previousView.length > 0
    goForward.enabled = nextView.length > 0
  }

  /** run a query (will open first object found) */
  def find(q: String): Unit = {
    objSearch.queryText := q
    objSearch.searchAction()
  }
  /** show a type and update navigation */
  def goTo(v: View): Unit = {
    nextView.clear()
    if (currentView != null && v.obj != currentView.obj) previousView.push(currentView)
    _goTo(v)
  }
  /** show previously shown type */
  val goBack = new swing.Action("Back") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("alt LEFT"))
    mnemonic = swing.event.Key.B.id
    icon = new qq.icons.BackIcon(true)
    smallIcon = new qq.icons.BackIcon(true, true)
    enabled = false
    override def apply() = {
      if (previousView.length > 0) {
        val t = previousView.pop()
        nextView.push(currentView)
        _goTo(t)
      }
    }
  }
  /** show next type */
  val goForward = new swing.Action("Forward") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("alt RIGHT"))
    mnemonic = swing.event.Key.F.id
    icon = new qq.icons.ForwardIcon(true)
    smallIcon = new qq.icons.ForwardIcon(true, true)
    enabled = false
    override def apply() = {
      if (nextView.length > 0) {
        val t = nextView.pop()
        previousView.push(currentView)
        _goTo(t)
      }
    }
  }

  val toggleSearchVisible = new swing.Action("Show Search") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl S"))
    mnemonic = swing.event.Key.S.id
    override def apply() = {
      updateVisibility
    }
  }
  val toggleGraphVisible = new swing.Action("Show Graph") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl G"))
    mnemonic = swing.event.Key.G.id
    override def apply() = {
      updateVisibility
    }
  }
  
val showTypeOfThisObject = new swing.Action("Show Type of Current Object") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl T"))
    mnemonic = swing.event.Key.T.id
    override def apply() = {
      qq.editor.Main.newTypeTab(file.s(currentView.obj.getTypeName))
    }  
  }  
  
  /* menu entries for the main window */
  override def viewMenuItems = Seq(
    new swing.MenuItem(goBack),
    new swing.MenuItem(goForward),
    new swing.CheckMenuItem("") {
      action = toggleSearchVisible
      peer.setModel(searchVisibleModel)
    },
    new swing.CheckMenuItem("") {
      action = toggleGraphVisible
      peer.setModel(graphVisibleModel)
    })
  override def typeMenuItems = Seq(
    new swing.MenuItem(showTypeOfThisObject))
  override def objectMenuItems = Seq(
    new qq.util.TodoMenuItem("…"))
  /* the layout */
  val toolBar = qq.util.Swing.HBox(
    new swing.Button(goBack) {
      text = ""
      icon = new qq.icons.BackIcon(true)
      disabledIcon = new qq.icons.BackIcon(false)
    },
    new swing.Button(goForward) {
      text = ""
      icon = new qq.icons.ForwardIcon(true)
      disabledIcon = new qq.icons.ForwardIcon(false)
    },
    new swing.ToggleButton("") {
      action = toggleSearchVisible
      peer.setModel(searchVisibleModel)
    },
    new swing.ToggleButton("") {
      action = toggleGraphVisible
      peer.setModel(graphVisibleModel)
    },
    scala.swing.Swing.HGlue)
  val objSearch = new SearchResults(this)
  val objEdit = qq.util.Swing.HBox()
  val objGraph = qq.util.Swing.HBox()

  val mainContent = qq.util.Swing.HBox()

  def updateVisibility: Unit = {
    mainContent.contents.clear()
    if (searchVisible) {
      if (graphVisible) {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(objSearch, new swing.SplitPane(swing.Orientation.Vertical) {
            contents_=(objEdit, objGraph)
          })
        }
      } else {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(objSearch, objEdit)
        }
      }
    } else {
      if (graphVisible) {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(objEdit, objGraph)
        }
      } else {
        mainContent.contents += objEdit
      }
    }
    mainContent.revalidate()
  }
  updateVisibility
  title = "Types"
  content = qq.util.Swing.VBox(toolBar, mainContent)
}