package qq.editor.queries

import ogss.common.scala.api;
import ogss.common.scala.internal;
import ogss.common.scala.internal.fieldTypes._;

/** The basic triple query `subject predicate object.`, i.e.\ in RDF terms:
 *  `subject` and `object` are `predicate`-related, or, here: `subject` has field `predicate`,
 *  and the value of `predicate` in `subject` is `object`.
 *  
 *  Implementation is in different subclasses depending on which of subj and pred are variables */
abstract class TripleQuery(
  file0: qq.editor.File,
  val subj: Term,
  val pred: Field,
  val obj: Term)
    extends Query(file0) {

  override def prepare(assigned: Seq[String]) = ()

}
object TripleQuery {
  def apply(
    file0: qq.editor.File,
    s: Term,
    p: Field,
    o: Term) = s match {
    case s: VarTerm ⇒
      o match {
        case o: VarTerm ⇒ new VarVarTripleQuery(file0, s, p, o)
        case o: ConstTerm ⇒ new VarConstTripleQuery(file0, s, p, o)
      }
    case s: ConstTerm ⇒
      o match {
        case o: VarTerm ⇒ new ConstVarTripleQuery(file0, s, p, o)
        case _          ⇒ throw new Exception("cons p const TODO")
      }
  }
}
/** Tripe Query `?v pred ?w`. */
class VarVarTripleQuery(
  file0: qq.editor.File,
  override val subj: VarTerm,
  override val pred: Field,
  override val obj: VarTerm)
    extends TripleQuery(file0, subj, pred, obj) {

  def variables = Seq(subj.variable, obj.variable)

  def find() = {
    /* scan all pools that have this field*/
    pred().flatMap {
      case (pool, field) ⇒
        field.t.asInstanceOf[internal.FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
            pool.map { x ⇒ Map(subj.variable -> x, obj.variable -> field.get(x.asInstanceOf[internal.Obj])) }
          case c: SingleArgumentType[c, e] ⇒
            pool.flatMap { so ⇒
              field.get(so.asInstanceOf[internal.Obj]).asInstanceOf[c]
                .map(elem ⇒ Map(subj.variable -> so, obj.variable -> elem))
            }
          case m: MapType[k, v] ⇒
            import qq.util.FlattenedMap._
            pool.flatMap { so => 
              val map = field.get(so.asInstanceOf[internal.Obj]).asInstanceOf[scala.collection.mutable.HashMap[k,v]]
              keys(map, m ).map(key => Map(subj.variable -> so, obj.variable -> get(map, m, key)))
            }
        }
    }
  }
  def find(assignment: Map[String, Any]) = {
    // subject must be bound (checked in costSizeEstimate), otherwise it could get expensive
    // if object is not bound, bind to all possible values, otherwise filter 
    val s = assignment(subj.variable)
    s match {
      case s: internal.Obj ⇒
        pred(s).flatMap {
          case (pool, field) ⇒
            field.t.asInstanceOf[internal.FieldType[_]] match {
              case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool ⇒
                val value = field.get(s.asInstanceOf[internal.Obj])
                if (assignment.contains(obj.variable)) {
                  if (assignment(obj.variable) == value) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  Iterator(assignment + (obj.variable -> value))
                }
              case _: internal.Pool[_] | _: internal.AnyRefType ⇒
                val o = field.get(s.asInstanceOf[internal.Obj])
                if (assignment.contains(obj.variable)) {
                  if (assignment(obj.variable) == o) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  if (o != null) {
                    Iterator(assignment + (obj.variable -> o))
                  } else {
                    Iterator()
                  }
                }
              case _: SingleArgumentType[c, e] ⇒
                val c = field.get(s.asInstanceOf[internal.Obj]).asInstanceOf[c]
                if (assignment.contains(obj.variable)) {
                  if (c.toSeq.contains(assignment(obj.variable))) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  c.map(elem ⇒ assignment + (obj.variable -> elem))
                }
          case m: MapType[k, v] ⇒
            import qq.util.FlattenedMap._
              val map = field.get(s.asInstanceOf[internal.Obj]).asInstanceOf[scala.collection.mutable.HashMap[k,v]]

                if (assignment.contains(obj.variable)) {
                  if (keys(map, m).map(key => get(map, m, key)).filter(_ == assignment(obj.variable)).size != 0) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  keys(map, m).map(key ⇒ assignment + (obj.variable -> get(map, m, key)))
                }
            }
        }
      case _ ⇒ Iterator()
    }

  }
  override def costSizeEstimate() = {
    var x = 0.0
    for ((pool, field) ← pred()) {
      field.t.asInstanceOf[internal.FieldType[_]] match {
        case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
          x += pool.size
        case c: ContainerType[c] ⇒
          x += 10 * pool.size
      }
    }
    (x, x)
  }

  override def costSizeEstimate(assignment: Seq[String]) = {
    if (!assignment.contains(subj.variable)) {
      (Double.PositiveInfinity, Double.PositiveInfinity)
    } else {
      var x = 0.0
      for ((pool, field) ← pred()) {
        field.t.asInstanceOf[internal.FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
            x = x max 1
          case c: ContainerType[c] ⇒
            x = x max 10
        }
      }
      if (assignment.contains(obj.variable)) {
        (x, 0.1)
      } else {
        (x, x)
      }
    }
  }
}

/** Triple query `c pred ?v`. */
class ConstVarTripleQuery(
  file0: qq.editor.File,
  override val subj: ConstTerm,
  override val pred: Field,
  override val obj: VarTerm)
    extends TripleQuery(file0, subj, pred, obj) {

  if (!subj.value.isInstanceOf[internal.Obj]) throw new Exception("constant subject must be a skill object")

  def variables = Seq(obj.variable)

  def find() = {
    find(Map())
  }
  def find(assignment: Map[String, Any]) = {
    // if object is not bound, bind to all possible values, otherwise filter 
    subj.value match {
      case s: internal.Obj ⇒
        pred(s).flatMap {
          case (pool, field) ⇒
            field.t.asInstanceOf[internal.FieldType[_]] match {
              case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool ⇒
                val value = field.get(s.asInstanceOf[internal.Obj])
                if (assignment.contains(obj.variable)) {
                  if (assignment(obj.variable) == value) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  Iterator(assignment + (obj.variable -> value))
                }
              case _: internal.Pool[_] | _: internal.AnyRefType ⇒
                val o = field.get(s.asInstanceOf[internal.Obj])
                if (assignment.contains(obj.variable)) {
                  if (assignment(obj.variable) == o) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  if (o != null) {
                    Iterator(assignment + (obj.variable -> o))
                  } else {
                    Iterator()
                  }
                }
              case _: SingleArgumentType[c, e] ⇒
                val c = field.get(s.asInstanceOf[internal.Obj]).asInstanceOf[c]
                (if (assignment.contains(obj.variable)) {
                  c.filter(_ == assignment(obj.variable))
                } else {
                  c
                }).map(elem ⇒ assignment + (obj.variable -> elem))
              case m: MapType[c, e] ⇒
                Iterator() // TODO
            }
        }
      case _ ⇒ Iterator()
    }
  }
  override def costSizeEstimate() = {
    costSizeEstimate(Seq())
  }

  override def costSizeEstimate(assignment: Seq[String]) = {
    var x = 0.0
    for ((pool, field) ← pred(subj.value.asInstanceOf[internal.Obj])) {
      field.t.asInstanceOf[internal.FieldType[_]] match {
        case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
          x += 1
        case c: ContainerType[c] ⇒
          x += 10
      }
    }
    if (assignment.contains(obj.variable)) {
      (x, 0.1)
    } else {
      (x, x)
    }
  }
}

/** Triple query `?v pred c.`. `*/
class VarConstTripleQuery(
  file0: qq.editor.File,
  override val subj: VarTerm,
  override val pred: Field,
  override val obj: ConstTerm)
    extends TripleQuery(file0, subj, pred, obj) {

  def variables = Seq(subj.variable)

  def find() = {
    /* scan all pools that have this field*/
    pred().flatMap {
      case (pool, field) ⇒
        field.t.asInstanceOf[internal.FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
            // x ⇒  ..  TODO is this okey?
            pool.filter(x ⇒ field.get(x.asInstanceOf[internal.Obj]) == obj.value).map { x ⇒ Map(subj.variable -> x) }
          case c: SingleArgumentType[c, e] ⇒
            pool.flatMap { so ⇒
              field.get(so.asInstanceOf[internal.Obj]).asInstanceOf[c]
                .filter(_ == obj.value).map(elem ⇒ Map(subj.variable -> so))
            }
          case m: MapType[c, e] ⇒
            Iterator() // TODO
        }
    }
  }
  def find(assignment: Map[String, Any]) = {
    // subject must be bound (checked in costSizeEstimate), otherwise it could get expensive
    // if object is not bound, bind to all possible values, otherwise filter 
    val s = assignment(subj.variable)
    s match {
      case s: internal.Obj ⇒
        pred(s).flatMap {
          case (pool, field) ⇒
            field.t.asInstanceOf[internal.FieldType[_]] match {
              case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool ⇒
                val value = field.get(s.asInstanceOf[internal.Obj])
                if (obj.value == value) {
                  Iterator(assignment)
                } else {
                  Iterator()
                }
              case _: internal.Pool[_] | _: internal.AnyRefType ⇒
                val o = field.get(s.asInstanceOf[internal.Obj])
                if (obj.value == o) {
                  Iterator(assignment)
                } else {
                  Iterator()
                }
              case _: SingleArgumentType[c, e] ⇒
                val c = field.get(s.asInstanceOf[internal.Obj]).asInstanceOf[c]
                if (c.toSeq.contains(obj.value)) {
                  Iterator(assignment)
                } else {
                  Iterator()
                }
              case m: MapType[c, e] ⇒
                Iterator() // TODO
            }
        }
      case _ ⇒ Iterator()
    }

  }
  override def costSizeEstimate() = {
    var x = 0.0
    for ((pool, field) ← pred()) {
      field.t.asInstanceOf[internal.FieldType[_]] match {
        case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
          x += pool.size
        case c: ContainerType[c] ⇒
          x += 10 * pool.size
      }
    }
    (x, x / 10)
  }

  override def costSizeEstimate(assignment: Seq[String]) = {
    if (!assignment.contains(subj.variable)) {
      (Double.PositiveInfinity, Double.PositiveInfinity)
    } else {
      var x = 0.0
      for ((pool, field) ← pred()) {
        field.t.asInstanceOf[internal.FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: internal.StringPool | Bool | _: internal.Pool[_] | _: internal.AnyRefType ⇒
            x = x max 1
          case c: ContainerType[c] ⇒
            x = x max 10
        }
      }
      (x, 0.1)
    }
  }
}
