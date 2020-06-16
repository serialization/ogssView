package ogss.view.empty.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.UIManager;

import ogss.common.java.api.OGSSException;
import ogss.view.empty.Main;
import ogss.view.empty.MainFrame;
import ogss.view.empty.Model;

public class LoadAction extends AbstractAction {

   public LoadAction() {
      super("Load", UIManager.getIcon("FileView.directoryIcon"));
   }

   @Override
   public void actionPerformed(ActionEvent e) {
      // file selection
      File f;
      {
         var fc = new JFileChooser();
         var status = fc.showOpenDialog(MainFrame.frame);
         if (JFileChooser.APPROVE_OPTION == status) {
            f = fc.getSelectedFile();
         } else {
            return;
         }
      }

      // load
      try {
         Model.load(f);
      } catch (OGSSException | IOException ex) {
         Main.report(ex);
      }
   }
}
