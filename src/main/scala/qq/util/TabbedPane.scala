package qq.util

object TabbedPane {
  class Page(parent0: TabbedPane, title0: String, content0: swing.Component, tip0: String)
      extends swing.TabbedPane.Page(parent0, title0, content0, tip0) {
    var tabbedPane = parent0
    val page = this
    def this(title0: String, content0: swing.Component, tip0: String) =
      this(null, title0, content0, tip0)
    def this(title0: String, content0: swing.Component) =
      this(title0, content0, "")
    val tabComponent = new swing.BoxPanel(swing.Orientation.Horizontal) {
      contents += new swing.Label(" "+title0)
      opaque = false
      listenTo(mouse.clicks)
      reactions += {
        case x: swing.event.MousePressed =>
          tabbedPane.peer.setSelectedComponent(content.peer)
      }
    }
    def updateTabComponent(): Unit = {
      tabComponent.contents.clear
      tabComponent.contents += new swing.Label(title)
    }
    private def updateTabComponent_(): Unit = if (tabComponent != null) updateTabComponent()
    override def title:String = super.title
    override def title_=(x: String):Unit = {super.title = x; updateTabComponent_}
    /// TODO why does peer.indexOfComponent(pg.content.peer) below work and this doesn't?
    override def index: Int = if (tabbedPane != null) tabbedPane.peer.indexOfComponent(content.peer) else 0;
  }
  class ClosablePage(parent0: TabbedPane, title0: String, content0: swing.Component, tip0: String)
      extends Page(parent0, title0, content0, tip0) {
    def this(title0: String, content0: swing.Component, tip0: String) =
      this(null, title0, content0, tip0)
    def this(title0: String, content0: swing.Component) =
      this(title0, content0, "")
    override def updateTabComponent(): Unit = {
      tabComponent.contents.clear
      tabComponent.contents += new swing.Label(title + " ") {opaque = false}
      tabComponent.contents += new PlainButton(swing.Action("close"){parent.pages.remove(index); onClose}) {
        this.icon = javax.swing.UIManager.getIcon("InternalFrame.closeIcon")
        this.text = ""
      }
    }
    def onClose(): Unit = {}
  }
}

class TabbedPane extends swing.TabbedPane {
  
  // TODO howto override pages?
  def addPage(pg: TabbedPane.Page) {
    pg.tabbedPane = this
    pages += pg
    peer.setTabComponentAt(pg.index, pg.tabComponent.peer)
  }
}
