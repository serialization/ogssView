package ogss.view.empty;

import java.io.File;
import java.io.IOException;

import ogss.common.java.api.Mode;
import ogss.common.java.api.OGSSException;

/**
 * The root of the model. An application can load at most one file.
 * 
 * @author Timm Felden
 */
public enum Model {
   INSTANCE;

   private static OGFile graph;

   public static OGFile graph() {
      return graph;
   }

   public static void load(File file) throws OGSSException, IOException {
      if (null == graph) {
         graph = OGFile.open(file, Mode.ReadOnly, Mode.Read);
      } else {
         throw new Error("yeah what ever TBD");
      }
   }
}
