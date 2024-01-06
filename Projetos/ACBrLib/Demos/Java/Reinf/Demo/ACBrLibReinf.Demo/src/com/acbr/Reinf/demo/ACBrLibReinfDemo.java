/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.Reinf.demo;

import javax.swing.ImageIcon;
import javax.swing.UIManager;

/**
 *
 * @author celso
 */
public class ACBrLibReinfDemo {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(FrmMain.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
               FrmMain frmMain = new FrmMain();
               frmMain.setIconImage(new ImageIcon(getClass().getResource("/resources/images/acbr_icon.png")).getImage());
               //frmMain.setResizable(false);
               //frmMain.setLocationRelativeTo(null);
               frmMain.setVisible(true);
            }
        });
    }
    
}
