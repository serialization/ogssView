package ogss.view.empty;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.WindowConstants;

import ogss.view.empty.actions.LoadAction;

public class MainFrame {

   public static JFrame frame;

   static void makeGUI() {
      // sanitize swing configuration
//      try {
//         // try to set gtk L&F, because it is HiDPI-aware
//         UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
//      } catch (ClassNotFoundException | InstantiationException
//            | IllegalAccessException | UnsupportedLookAndFeelException e) {
//         e.printStackTrace();
//      }
      System.setProperty("awt.useSystemAAFontSettings", "on");
      System.setProperty("swing.aatext", "true");
      System.setProperty("sun.java2d.dpiaware", "true");

      // create default window
      frame = new JFrame("OGSS View");
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

      // add content
      makeMenuBar();

      frame.add(new JLabel("drop a file to open"));

      // display
      frame.setMinimumSize(new Dimension(800, 600));
      frame.pack();
      frame.setLocationRelativeTo(null);
      frame.setVisible(true);
   }

   private static void makeMenuBar() {
      JMenuBar mb = new JMenuBar();

      {
         JMenu file = new JMenu("file");
         file.add(new LoadAction());
         mb.add(file);
      }

      JMenu view = new JMenu("view");
      mb.add(view);

      frame.setJMenuBar(mb);
   }
}
