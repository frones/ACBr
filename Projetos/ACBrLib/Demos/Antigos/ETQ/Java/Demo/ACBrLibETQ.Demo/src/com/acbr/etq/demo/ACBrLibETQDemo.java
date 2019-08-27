/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.etq.demo;

import javax.swing.ImageIcon;
import javax.swing.UIManager;

/**
 *
 * @author rften
 */
public class ACBrLibETQDemo {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
         try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(FrmMain.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        java.awt.EventQueue.invokeLater(() -> {
            FrmMain frame = new FrmMain();
            frame.setIconImage(new ImageIcon(FrmMain.class.getResource( "/resources/images/acbr_icon.png" )).getImage());
            frame.setResizable(false);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
         });
    }
    
}
