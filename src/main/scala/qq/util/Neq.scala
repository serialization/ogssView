package qq.util

object Neq {
  
  // ≠ that gets rid of the special treatment of floats (0.0==-0.0 and NaN!=NaN)
  def apply[T](l: T, r: T): Boolean = {
    if (l == null) {
      r != null
    } else {
      !l.equals(r)
    }
  }  
  
}