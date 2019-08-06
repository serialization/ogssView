/*   ____  _____________                                                                                              *\
 *  / __ \/ ___/ __/ __/  Your OGSS/Scala Binding                                                                     * 
 * / /_/ / (_\_\ \_\ \    <<debug>>                                                                                   * 
 * \____/\___/___/___/    by: <<some developer>>                                                                      * 
\*                                                                                                                    */
package empty;

import java.io.File;
import java.nio.file.Path;

import ogss.common.scala.api.Create
import ogss.common.scala.api.Mode;
import ogss.common.scala.api.OGSSException;
import ogss.common.scala.api.ReadMode
import ogss.common.scala.api.Read
import ogss.common.scala.api.WriteMode
import ogss.common.scala.api.Write
import ogss.common.scala.internal.StateInitializer;

/**
 * An abstract OGSS file that is hiding all the dirty implementation details
 * from you.
 *
 * @note Type access fields start with a capital letter to avoid collisions and to match type names.
 *
 * @author Timm Felden
 */
final class OGFile private (_init : StateInitializer)
  extends ogss.common.scala.internal.State(_init : StateInitializer) {


  _init.awaitResults
}

/**
 * @author Timm Felden
 */
object OGFile {
  /**
   * Reads a binary OGSS file and turns it into an OGSS state.
   */
  def open(path : String, read : ReadMode = Read, write : WriteMode = Write) : OGFile = {
    val f = new File(path)
    if (!f.exists())
      f.createNewFile()
    readFile(f.toPath, read, write)
  }
  /**
   * Reads a binary OGSS file and turns it into an OGSS state.
   */
  def open(file : File, read : ReadMode, write : WriteMode) : OGFile = {
    if (!file.exists())
      file.createNewFile()
    readFile(file.toPath, read, write)
  }
  /**
   * Reads a binary OGSS file and turns it into an OGSS state.
   */
  def open(path : Path, read : ReadMode, write : WriteMode) : OGFile = readFile(path, read, write)

  /**
   * same as open(create)
   */
  def create(path : Path, write : WriteMode = Write) : OGFile = readFile(path, Create, write)

  /**
   * same as open(read)
   */
  def read(path : Path, write : WriteMode = Write) : OGFile = readFile(path, Read, write)

  private def readFile(path : Path, read : ReadMode, write : WriteMode) : OGFile =
    new OGFile(StateInitializer(path, internal.PB, Seq(read, write)))
}
