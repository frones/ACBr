/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package br.acbr.bal.demo;

import javax.swing.UIManager;

/**
 *
 * @author celso
 */
public class ACBrLibBALDemo {

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
            FrmMain frmMain = new FrmMain();
            frmMain.setResizable(false);
            frmMain.setLocationRelativeTo(null);
            frmMain.setVisible(true);
        });
    }
    
}
