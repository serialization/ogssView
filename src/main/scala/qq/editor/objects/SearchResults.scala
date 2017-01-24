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
      } catch {
        case e: Exception ⇒
          val text = e.getMessage
          queryError.text = if (text != null) text else e.toString()
          queryError.visible = true
      }
      queryError.revalidate()
    }
  }
  def showPage: Unit = {
    readResults
    if (resultsTable != null) { deafTo(resultsTable.selection) }

    resultsTable = new swing.Table(
      resultsRetrieved
        .drop(displayOffset)
        .take(pageLength)
        .map { x ⇒ x.values.toArray }.toArray,
      query.variables) {
    //  model = new javax.swing.table.DefaultTableModel() {
    //    override def isCellEditable(row: Int, col: Int) = false
    //  }
    }
    listenTo(resultsTable.selection)

    resultsPart.contents.clear()
    resultsPart.contents += resultsTable

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
  import qq.util.Swing._;
  contents += VBox(
    HBox(queryEdit, new swing.Button(searchAction)),
    queryError,
    resultsPart)
}