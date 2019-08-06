/*   ____  _____________                                                                                              *\
 *  / __ \/ ___/ __/ __/  Your OGSS/Scala Binding                                                                     * 
 * / /_/ / (_\_\ \_\ \    <<debug>>                                                                                   * 
 * \____/\___/___/___/    by: <<some developer>>                                                                      * 
\*                                                                                                                    */
package empty;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.HashMap;
import java.util.HashSet;

import scala.language.existentials

import ogss.common.scala.api.OGSSException;
import ogss.common.scala.internal.AnyRefType;
import ogss.common.scala.internal.EnumPool;
import ogss.common.scala.internal.EnumProxy;
import ogss.common.scala.internal.FieldType;
import ogss.common.scala.internal.HullType;
import ogss.common.scala.internal.Obj;
import ogss.common.scala.internal.Pool;
import ogss.common.scala.internal.StringPool;
import ogss.common.scala.internal.SubPool;
import ogss.common.scala.internal.fields.AutoField
import ogss.common.scala.internal.fields.KnownField
import ogss.common.streams.BufferedOutStream;
import ogss.common.streams.FileInputStream;
import ogss.common.streams.MappedInStream;

object internal {

  object PB extends ogss.common.scala.internal.PoolBuilder(10) {

    override def literals : Array[String] = Array()

    override def kcc(ID : scala.Int) : scala.Int = ID match {
      case _ ⇒ -1
    }

    override def name(ID : scala.Int) : String = null

    override def make(ID : scala.Int, index : scala.Int) : Pool[_  <: Obj] = null

    override def enumName(ID : scala.Int) : String = null

    override def enumMake(ID : scala.Int) : Enumeration = null
  }
}
