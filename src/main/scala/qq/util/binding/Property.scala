package qq.util.binding

import scala.collection.mutable;
import de.ust.skill.common.scala.api

/** For binding GUI elements to actual data. Adapted from schauderhaft blog */
class Property[T](val owner: PropertyOwner, val name: String, var value: T)
    extends Observable[T]() {

  owner.properties = owner.properties :+ this
  
  onChange.strong += (_ => owner.doOnAnyPropertyChange)

  val description: String = name
  def apply(): T = value
  def :=(newValue: T): Unit = {
    validationMessages(newValue) match {
      case x :: xs =>
        throw new RestrictionException(x)
      case Nil =>
        if (value != newValue) {
          value = newValue
          doOnChange(newValue)
        }
    }
  }
  /** Restrictions on the value of this property. */
  val restrictions: mutable.ListBuffer[Restriction[T]] = mutable.ListBuffer()

  /**
   * @retval Right(()) when @c value satisfies all ::restrictions set for this property
   *  @retval Left(errormessage) with the error message of one of the restrictions violated otherwise
   */
  def validationMessages(value: T): List[String] = {
    restrictions.toList.flatMap(_.validationMessage(value))
  }

  def defaultEditor: EditControl[T] = {
    value match {
      case _: Boolean => new BoolEdit(this.asInstanceOf[Property[Boolean]]).asInstanceOf[EditControl[T]]
      case _: Byte   => new TextEdit(this.asInstanceOf[Property[Byte]], _.toByte).asInstanceOf[EditControl[T]]
      case _: Int    => new TextEdit(this.asInstanceOf[Property[Int]], _.toInt).asInstanceOf[EditControl[T]]
      case _: Long   => new TextEdit(this.asInstanceOf[Property[Long]], _.toLong).asInstanceOf[EditControl[T]]
      case _: Float  => new TextEdit(this.asInstanceOf[Property[Float]], _.toFloat).asInstanceOf[EditControl[T]]
      case _: Double => new TextEdit(this.asInstanceOf[Property[Double]], _.toDouble).asInstanceOf[EditControl[T]]
      case _: String => new TextEdit(this.asInstanceOf[Property[String]], x => x).asInstanceOf[EditControl[T]]
// TODO
//      case _: scala.collection.mutable.Buffer[_] => new ListEdit(this.asInstanceOf[Property[scala.collection.mutable.Buffer[T]]]).asInstanceOf[EditControl[T]]
//      case _ => new NoEdit(this)
    }
  }
}