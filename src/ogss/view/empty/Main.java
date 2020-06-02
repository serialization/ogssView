package ogss.view.empty;

import java.awt.Font;
import java.util.Arrays;
import java.util.Set;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

/**
 * Entrypoint for the viewer. We allow exactly one file to be viewed at a time.
 * 
 * @author Timm Felden
 */
public class Main {

   private static JFrame frame;

   public static void main(String[] args) {
      if (args.length > 0) {

      }
      SwingUtilities.invokeLater(Main::makeGUI);

      // the AWT thread will keep us alive from now on
   }

   private static void makeGUI() {
      // sanitize swing configuration
      try {
         // try to set gtk L&F, because it is HiDPI-aware
         UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
      } catch (ClassNotFoundException | InstantiationException
            | IllegalAccessException | UnsupportedLookAndFeelException e) {
         e.printStackTrace();
      }
      System.setProperty("awt.useSystemAAFontSettings","on");
      System.setProperty("swing.aatext", "true");
      System.setProperty("sun.java2d.dpiaware", "true");

      // create default window
      frame = new JFrame("OGSS View");
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);      
      

      // add content
      makeMenuBar();

      frame.add(new JLabel("Happy as a pig in a blanket"));

      // display
      frame.pack();
      frame.setVisible(true);
   }
   
   public static void setDefaultSize(int size) {

      Set<Object> keySet = UIManager.getLookAndFeelDefaults().keySet();
      Object[] keys = keySet.toArray(new Object[keySet.size()]);

      for (Object key : keys) {

          if (key != null && key.toString().toLowerCase().contains("font")) {

              System.out.println(key);
              Font font = UIManager.getDefaults().getFont(key);
              if (font != null) {
                  font = font.deriveFont((float)size);
                  UIManager.put(key, font);
              }

          }

      }

  }


   private static void makeMenuBar() {
      JMenuBar mb = new JMenuBar();

      JMenu file = new JMenu("file");
      mb.add(file);

      JMenu view = new JMenu("view");
      mb.add(view);

      frame.setJMenuBar(mb);
   }
}
