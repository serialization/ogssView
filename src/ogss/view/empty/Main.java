package ogss.view.empty;

import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

/**
 * Entrypoint for the viewer. We allow exactly one file to be viewed at a time.
 * 
 * @author Timm Felden
 */
public class Main {

   public static void main(String[] args) {
      if (args.length > 0) {

      }
      SwingUtilities.invokeLater(MainFrame::makeGUI);

      // the AWT thread will keep us alive from now on
   }

   /**
    * Report an exception as dialog to the user
    */
   public static void report(Throwable ex) {
      StringWriter sw = new StringWriter();
      try (PrintWriter pw = new PrintWriter(sw)) {
         ex.printStackTrace(pw);
      }

      JOptionPane.showMessageDialog(MainFrame.frame, sw.toString(),
            "Unhandeled Exception", JOptionPane.ERROR_MESSAGE);

   }
}
