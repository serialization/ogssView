package qq.editor.types

import de.ust.skill.common.scala.api;
import scala.collection.mutable;
import scala.swing;

/**
 * A page displaying data about a type, with an optional type tree for naviagation on the
 * left and an option graph representation on the right
 */
class TypePage(val file: qq.editor.File) extends qq.editor.Page {

  /** types tree visible*/
  def treeVisible: Boolean = treeVisibleModel.isSelected()
  val treeVisibleModel = new javax.swing.JToggleButton.ToggleButtonModel()
  treeVisibleModel.setSelected(true)

  /** graph visible */
  def graphVisible: Boolean = graphVisibleModel.isSelected()
  val graphVisibleModel = new javax.swing.JToggleButton.ToggleButtonModel()

  /** the type that is currently shown */
  var currentType: api.Access[_] = null

  /** previously shown types (for back navigation) */
  val previousType: mutable.Stack[api.Access[_]] = new mutable.Stack()

  /** previously previously shown types :) (for forward navigation) */
  val nextType: mutable.Stack[api.Access[_]] = new mutable.Stack()

  /** show a type (internal, for goTo, goBack, goForward) */
  private def _goTo(t: api.Access[_]): Unit = {
    currentType = t
    title = t.name
    typeEdit.contents.clear()
    typeEdit.contents += new TypeEdit(this, t)
    typeTree.select(t)
    goBack.enabled = previousType.length > 0
    goForward.enabled = nextType.length > 0
  }

  /** show a type and update navigation */
  def goTo(t: api.Access[_]): Unit = {
    nextType.clear()
    if (currentType != null && t != currentType) previousType.push(currentType)
    _goTo(t)
  }
  /** show previously shown type */
  val goBack = new swing.Action("Back") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl B"))
    mnemonic = swing.event.Key.B.id
    icon = new qq.icons.BackIcon(true)
    smallIcon = new qq.icons.BackIcon(true, true)
    enabled = false
    override def apply() = {
      if (previousType.length > 0) {
        val t = previousType.pop()
        nextType.push(currentType)
        _goTo(t)
      }
    }
  }
  /** show next type */
  val goForward = new swing.Action("Forward") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl F"))
    mnemonic = swing.event.Key.F.id
    icon = new qq.icons.ForwardIcon(true)
    smallIcon = new qq.icons.ForwardIcon(true, true)
    enabled = false
    override def apply() = {
      if (nextType.length > 0) {
        val t = nextType.pop()
        previousType.push(currentType)
        _goTo(t)
      }
    }
  }

  val toggleTreeVisible = new swing.Action("Show Tree") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl T"))
    mnemonic = swing.event.Key.T.id
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
  /* menu entries for the main window */
  override def viewMenuItems = Seq(
    new swing.MenuItem(goBack),
    new swing.MenuItem(goForward),
    new swing.CheckMenuItem("") {
      action = toggleTreeVisible
      peer.setModel(treeVisibleModel)
    },
    new swing.CheckMenuItem("") {
      action = toggleGraphVisible
      peer.setModel(graphVisibleModel)
    })
  override def typeMenuItems = Seq(
    new qq.util.TodoMenuItem("Go to Parent"))
  override def objectMenuItems = Seq(
    new qq.util.TodoMenuItem("Show Objects of Current Type"))
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
      action = toggleTreeVisible
      peer.setModel(treeVisibleModel)
    },
    new swing.ToggleButton("") {
      action = toggleGraphVisible
      peer.setModel(graphVisibleModel)
    },
    scala.swing.Swing.HGlue)
  val typeTree = new TypeTree(this)
  val typeEdit = qq.util.Swing.HBox()
  val typeGraph = new swing.Label("Todo graph")

  val mainContent = qq.util.Swing.HBox()

  def updateVisibility: Unit = {
    mainContent.contents.clear()
    if (treeVisible) {
      if (graphVisible) {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(typeTree, new swing.SplitPane(swing.Orientation.Vertical) {
            contents_=(typeEdit, typeGraph)
          })
        }
      } else {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(typeTree, typeEdit)
        }
      }
    } else {
      if (graphVisible) {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(typeEdit, typeGraph)
        }
      } else {
        mainContent.contents += typeEdit
      }
    }
    mainContent.revalidate()
  }
  updateVisibility
  title = "Types"
  content = qq.util.Swing.VBox(toolBar, mainContent)
}