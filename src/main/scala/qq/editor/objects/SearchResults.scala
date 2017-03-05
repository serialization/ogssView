package qq.editor.objects

import scala.collection.mutable;
import de.ust.skill.common.scala.api;

class SearchResults(val page: ObjectPage)
    extends swing.BoxPanel(swing.Orientation.Vertical)
    with qq.util.binding.PropertyOwner {

  val queryText = new qq.util.binding.Property[String](this, "Query", "")
  val queryEdit = queryText.defaultEditor

  val queryError = new swing.Label() { visible = false }

  var query: qq.editor.queries.Query = null
  var resultsIterator: Iterator[Map[String, Any]] = null
  var resultsRetrieved: mutable.ArrayBuffer[Map[String, Any]] = null

  var displayOffset: Int = 0
  var pageLength: Int = 50

  var resultsTable: swing.Table = null
  val resultsPart = new swing.BoxPanel(swing.Orientation.Vertical)

  /**
   * read enough elements from resultsIterator to show current page
   */
  private def readResults() = {
    while (resultsIterator.hasNext
      && resultsRetrieved.size < displayOffset + pageLength) {
      resultsRetrieved += resultsIterator.next
    }
  }

  val searchAction = new swing.Action("Search") {
    override def apply() = {
      if (queryEdit.isModified()) queryEdit.componentToProperty()
      try {
        query = qq.editor.queries.Query.parse(page.file, queryText())
        queryError.visible = false
        resultsIterator = query.find()
        resultsRetrieved = new mutable.ArrayBuffer()
        displayOffset = 0
        showPage
        if (resultsRetrieved.size > 0) {
          // open first result automatically
          resultsTable.peer.setRowSelectionInterval(0, 0)
        }
        if (resultsRetrieved.size == 1) {
          page.searchVisibleModel.setSelected(false)
          page.updateVisibility
        }
      } catch {
        case e: Exception ⇒
          val text = e.getMessage
          queryError.text = if (text != null) text else e.toString()
          queryError.visible = true
      }
      queryError.revalidate()
    }
  }
  queryEdit.property.onChange.strong += (_ ⇒ searchAction())

  val lblPagePos = new swing.Label("-")

  def showPage: Unit = {
    readResults
    if (resultsTable != null) { deafTo(resultsTable.selection) }

    val visibleData = resultsRetrieved
      .drop(displayOffset)
      .take(pageLength)
    resultsTable = new swing.Table(
      visibleData
        .map { x ⇒
          x.values.map { x ⇒
            if (x.isInstanceOf[api.SkillObject])
              page.file.idOfObj(x.asInstanceOf[api.SkillObject])
            else if (x == null) "(null)" else x.toString().asInstanceOf[Any] // no boolean checkbox magic
          }.toArray
        }.toArray,
      query.variables) {
      //  model = new javax.swing.table.DefaultTableModel() {
      //    override def isCellEditable(row: Int, col: Int) = false
      //  }
    }
    listenTo(resultsTable.selection)

    resultsPart.contents.clear()
    resultsPart.contents += new swing.ScrollPane(resultsTable)

    lblPagePos.text = s"$displayOffset to ${displayOffset + visibleData.size - 1}"
    pgUpAct.enabled = displayOffset > 0
    pgDnAct.enabled = displayOffset + pageLength <= resultsRetrieved.size || resultsIterator.hasNext
  }

  reactions += {
    case swing.event.TableRowsSelected(source, range, false) ⇒
      val i = source.selection.rows.leadIndex
      if (i >= 0) {
        val row = resultsRetrieved(displayOffset + i)
          .values
          .filter(_.isInstanceOf[api.SkillObject])
          .map(_.asInstanceOf[api.SkillObject])
        page.goTo(new page.View(row.head, row.drop(1)))
      }
  }

  private val pgDnAct = new swing.Action("Next Page") {
    icon = new qq.icons.ForwardIcon(true, false)
    override def apply() {
      if (resultsIterator.hasNext) {
        displayOffset += pageLength
        showPage
      }
    }
  }
  private val pgUpAct = new swing.Action("Previous Page") {
    icon = new qq.icons.BackIcon(true, false)
    override def apply() {
      if (displayOffset > 0) {
        displayOffset -= displayOffset min pageLength
        showPage
      }
    }
  }

  private val pgUpBtn = new qq.util.PlainButton(pgUpAct) {
    text = ""
    this.disabledIcon = new qq.icons.BackIcon(false, false)
  }
  private val pgDnBtn = new qq.util.PlainButton(pgDnAct) {
    text = ""
    this.disabledIcon = new qq.icons.ForwardIcon(false, false)
  }

  pgUpAct.enabled = false
  pgUpAct.enabled = false

  import qq.util.Swing._;
  import swing.Swing.HGlue
  contents += VBoxD(
    HBoxD(queryEdit, new swing.Button(searchAction)),
    queryError,
    HBoxD(HGlue, lblPagePos, HGlue, pgUpBtn, pgDnBtn),
    resultsPart)
}