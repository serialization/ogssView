package qq.editor.queries

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;

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
        field.t.asInstanceOf[FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
            pool.map { x ⇒ Map(subj.variable -> x, obj.variable -> x.asInstanceOf[api.SkillObject].get(field)) }
          case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
          case c: SingleBaseTypeContainer[c, e] ⇒
            pool.flatMap { so ⇒
              so.asInstanceOf[api.SkillObject].get(field).asInstanceOf[c]
                .map(elem ⇒ Map(subj.variable -> so, obj.variable -> elem))
            }
          case m: MapType[k, v] ⇒
            import qq.util.FlattenedMap._
            pool.flatMap { so => 
              val map = so.asInstanceOf[api.SkillObject].get(field).asInstanceOf[scala.collection.mutable.HashMap[k,v]]
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
      case s: api.SkillObject ⇒
        pred(s).flatMap {
          case (pool, field) ⇒
            field.t.asInstanceOf[FieldType[_]] match {
              case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType ⇒
                val value = s.asInstanceOf[api.SkillObject].get(field)
                if (assignment.contains(obj.variable)) {
                  if (assignment(obj.variable) == value) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  Iterator(assignment + (obj.variable -> value))
                }
              case _: UserType[_] | _: AnnotationType ⇒
                val o = s.asInstanceOf[api.SkillObject].get(field)
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
              case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
              case _: SingleBaseTypeContainer[c, e] ⇒
                val c = s.asInstanceOf[api.SkillObject].get(field).asInstanceOf[c]
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
              val map = s.asInstanceOf[api.SkillObject].get(field).asInstanceOf[scala.collection.mutable.HashMap[k,v]]

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
      field.t.asInstanceOf[FieldType[_]] match {
        case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
          x += pool.size
        case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
        case c: CompoundType[c] ⇒
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
        field.t.asInstanceOf[FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
            x = x max 1
          case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
          case c: CompoundType[c] ⇒
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

class ConstVarTripleQuery(
  file0: qq.editor.File,
  override val subj: ConstTerm,
  override val pred: Field,
  override val obj: VarTerm)
    extends TripleQuery(file0, subj, pred, obj) {

  if (!subj.value.isInstanceOf[api.SkillObject]) throw new Exception("constant subject must be a skill object")

  def variables = Seq(obj.variable)

  def find() = {
    find(Map())
  }
  def find(assignment: Map[String, Any]) = {
    // if object is not bound, bind to all possible values, otherwise filter 
    subj.value match {
      case s: api.SkillObject ⇒
        pred(s).flatMap {
          case (pool, field) ⇒
            field.t.asInstanceOf[FieldType[_]] match {
              case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType ⇒
                val value = s.asInstanceOf[api.SkillObject].get(field)
                if (assignment.contains(obj.variable)) {
                  if (assignment(obj.variable) == value) {
                    Iterator(assignment)
                  } else {
                    Iterator()
                  }
                } else {
                  Iterator(assignment + (obj.variable -> value))
                }
              case _: UserType[_] | _: AnnotationType ⇒
                val o = s.asInstanceOf[api.SkillObject].get(field)
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
              case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
              case _: SingleBaseTypeContainer[c, e] ⇒
                val c = s.asInstanceOf[api.SkillObject].get(field).asInstanceOf[c]
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
    for ((pool, field) ← pred(subj.value.asInstanceOf[api.SkillObject])) {
      field.t.asInstanceOf[FieldType[_]] match {
        case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
          x += 1
        case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
        case c: CompoundType[c] ⇒
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
        field.t.asInstanceOf[FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
            pool.filter(_.asInstanceOf[api.SkillObject].get(field) == obj.value).map { x ⇒ Map(subj.variable -> x) }
          case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
          case c: SingleBaseTypeContainer[c, e] ⇒
            pool.flatMap { so ⇒
              so.asInstanceOf[api.SkillObject].get(field).asInstanceOf[c]
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
      case s: api.SkillObject ⇒
        pred(s).flatMap {
          case (pool, field) ⇒
            field.t.asInstanceOf[FieldType[_]] match {
              case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType ⇒
                val value = s.asInstanceOf[api.SkillObject].get(field)
                if (obj.value == value) {
                  Iterator(assignment)
                } else {
                  Iterator()
                }
              case _: UserType[_] | _: AnnotationType ⇒
                val o = s.asInstanceOf[api.SkillObject].get(field)
                if (obj.value == o) {
                  Iterator(assignment)
                } else {
                  Iterator()
                }
              case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
              case _: SingleBaseTypeContainer[c, e] ⇒
                val c = s.asInstanceOf[api.SkillObject].get(field).asInstanceOf[c]
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
      field.t.asInstanceOf[FieldType[_]] match {
        case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
          x += pool.size
        case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
        case c: CompoundType[c] ⇒
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
        field.t.asInstanceOf[FieldType[_]] match {
          case I8 | I16 | I32 | I64 | V64 | F32 | F64 | _: StringType | BoolType | _: UserType[_] | _: AnnotationType ⇒
            x = x max 1
          case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒ Iterator()
          case c: CompoundType[c] ⇒
            x = x max 10
        }
      }
      (x, 0.1)
    }
  }
}
