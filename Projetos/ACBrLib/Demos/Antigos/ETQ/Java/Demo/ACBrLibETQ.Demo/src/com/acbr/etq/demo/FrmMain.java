/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.etq.demo;

import com.acbr.etq.ACBrETQ;
import com.sun.jna.ptr.IntByReference;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

/**
 *
 * @author rften
 */
public class FrmMain extends javax.swing.JFrame {
    
    private ACBrETQ acbrETQ;

    /**
     * Creates new form FrmMain
     */
    public FrmMain() {
        initComponents();
    }
    
    private void loadConfig() throws Exception {
        int ret;
        ByteBuffer buffer;
        IntByReference bufferLen;

        ret = acbrETQ.ETQ_ConfigLer(ACBrETQ.toUTF8(""));
        ACBrETQ.checkResult(ret);
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Porta"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        cmbPorta.setSelectedItem(ACBrETQ.fromUTF8(buffer, bufferLen.getValue()));
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Modelo"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        cmbModelo.setSelectedIndex(Integer.parseInt(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));  
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Temperatura"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        nudTemperatura.setValue(Integer.parseInt(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("DPI"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        cmbDPi.setSelectedIndex(Integer.parseInt(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Velocidade"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        nudVelocidade.setValue(Integer.parseInt(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("BackFeed"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        cmbBackFeed.setSelectedIndex(Integer.parseInt(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Avanco"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        nudAvancoEtq.setValue(Integer.parseInt(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));
        
        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = acbrETQ.ETQ_ConfigLerValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("LimparMemoria"), buffer, bufferLen);
        ACBrETQ.checkResult(ret);
        
        chkLimparMemoria.setSelected("1".equals(ACBrETQ.fromUTF8(buffer, bufferLen.getValue())));
    }
    
    private void saveConfig() throws Exception {
        int ret;

        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Porta"),
                cmbPorta.getSelectedItem().toString());
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Modelo"),
                Integer.toString(cmbModelo.getSelectedIndex()));
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Temperatura"),
                Integer.toString((Integer)nudTemperatura.getValue()));
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("DPI"),
                Integer.toString(cmbDPi.getSelectedIndex()));
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Velocidade"),
                Integer.toString((Integer)nudVelocidade.getValue()));
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("BackFeed"),
                Integer.toString(cmbBackFeed.getSelectedIndex()));
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("Avanco"),
                Integer.toString((Integer)nudAvancoEtq.getValue()));
        ACBrETQ.checkResult(ret);
        
        ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("ETQ"), ACBrETQ.toUTF8("LimparMemoria"),
                chkLimparMemoria.isSelected() ? "1" : "0");
        ACBrETQ.checkResult(ret);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        cmbPorta = new javax.swing.JComboBox<>();
        cmbModelo = new javax.swing.JComboBox();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        nudTemperatura = new javax.swing.JSpinner();
        cmbDPi = new javax.swing.JComboBox();
        jLabel4 = new javax.swing.JLabel();
        nudVelocidade = new javax.swing.JSpinner();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        cmbBackFeed = new javax.swing.JComboBox();
        nudAvancoEtq = new javax.swing.JSpinner();
        jLabel7 = new javax.swing.JLabel();
        chkLimparMemoria = new javax.swing.JCheckBox();
        jPanel2 = new javax.swing.JPanel();
        pnlImagem = new javax.swing.JPanel();
        txtImagem = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        btnCarregarImagem = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        nudCopias = new javax.swing.JSpinner();
        btnEtqSimples = new javax.swing.JButton();
        btnEtq3Colunas = new javax.swing.JButton();
        btnEtqBloco = new javax.swing.JButton();
        btnEtqImagem = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("ACBrLibETQ Demo");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowActivated(java.awt.event.WindowEvent evt) {
                formWindowActivated(evt);
            }
        });

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Configurações da Impressora"));

        jLabel1.setText("Porta");

        cmbPorta.setEditable(true);
        cmbPorta.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "COM1", "COM2", "LPT1", "LPT2", "\\\\localhost\\Epson", "c:\\temp\\ecf.txt", "TCP:192.168.0.31:9100" }));
        cmbPorta.setSelectedItem("c:\\temp\\ecf.txt");
        cmbPorta.setToolTipText("");

        cmbModelo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "etqNenhum", "etqPpla", "etqPplb", "etqZPLII", "etqEpl2" }));
        cmbModelo.setToolTipText("");

        jLabel2.setText("Modelo");

        jLabel3.setText("Temperatura");

        nudTemperatura.setValue(10);

        cmbDPi.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "dpi203", "dpi300", "dpi600" }));
        cmbDPi.setToolTipText("");

        jLabel4.setText("DPI Impressora");

        nudVelocidade.setValue(-1);

        jLabel5.setText("Velocidade");

        jLabel6.setText("BackFeed");

        cmbBackFeed.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "bfNone", "bfOn", "bfOff" }));
        cmbBackFeed.setToolTipText("");

        nudAvancoEtq.setValue(600);

        jLabel7.setText("Avanço Etiqueta");

        chkLimparMemoria.setText("Limpar Memoria");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(nudTemperatura)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(cmbPorta, javax.swing.GroupLayout.PREFERRED_SIZE, 127, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addComponent(jLabel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(nudVelocidade)
                    .addComponent(jLabel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(nudAvancoEtq)
                    .addComponent(jLabel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(cmbModelo, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(cmbDPi, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jLabel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jLabel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(cmbBackFeed, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jLabel6, javax.swing.GroupLayout.DEFAULT_SIZE, 128, Short.MAX_VALUE)))
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(chkLimparMemoria)
                .addGap(0, 0, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cmbModelo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(cmbPorta, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(nudTemperatura, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cmbDPi, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(nudVelocidade, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cmbBackFeed, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel7)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(nudAvancoEtq, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(chkLimparMemoria))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Carregar Imagem"));

        pnlImagem.setBackground(new java.awt.Color(255, 255, 255));
        pnlImagem.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        javax.swing.GroupLayout pnlImagemLayout = new javax.swing.GroupLayout(pnlImagem);
        pnlImagem.setLayout(pnlImagemLayout);
        pnlImagemLayout.setHorizontalGroup(
            pnlImagemLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        pnlImagemLayout.setVerticalGroup(
            pnlImagemLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        txtImagem.setText("ACBR");

        jLabel8.setText("Nome Imagem Memória");
        jLabel8.setToolTipText("");

        btnCarregarImagem.setText("Carregar Imagem");
        btnCarregarImagem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnCarregarImagemActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(txtImagem)
            .addComponent(pnlImagem, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel8)
                    .addComponent(btnCarregarImagem, javax.swing.GroupLayout.PREFERRED_SIZE, 209, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(0, 130, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(pnlImagem, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel8)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(txtImagem, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(btnCarregarImagem, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Impressão"));

        jLabel9.setText("Nº de Copias");

        nudCopias.setValue(1);

        btnEtqSimples.setText("Etiqueta Simples");
        btnEtqSimples.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnEtqSimplesActionPerformed(evt);
            }
        });

        btnEtq3Colunas.setText("Etiqueta 3 Colunas");
        btnEtq3Colunas.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnEtq3ColunasActionPerformed(evt);
            }
        });

        btnEtqBloco.setText("Bloco de Etiquetas");
        btnEtqBloco.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnEtqBlocoActionPerformed(evt);
            }
        });

        btnEtqImagem.setText("Imprimir Imagem");
        btnEtqImagem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnEtqImagemActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(nudCopias)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jLabel9)
                    .addComponent(btnEtqSimples, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnEtq3Colunas, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnEtqBloco, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnEtqImagem, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGap(0, 0, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jLabel9)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(nudCopias, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnEtqSimples)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnEtq3Colunas)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnEtqBloco)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(btnEtqImagem))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void formWindowActivated(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowActivated
        try {
            acbrETQ = ACBrETQ.INSTANCE;
            File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
            if(!iniFile.exists()) iniFile.createNewFile();
            
            int ret = acbrETQ.ETQ_Inicializar(ACBrETQ.toUTF8(iniFile.getAbsolutePath()), ACBrETQ.toUTF8(""));
            ACBrETQ.checkResult(ret);         

            Path pathLog = Paths.get(System.getProperty("user.dir"), "Docs");
            if (!Files.isDirectory(pathLog)) {
                pathLog.toFile().mkdirs();
            }

            ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("Principal"), ACBrETQ.toUTF8("LogNivel"), "4");
            ACBrETQ.checkResult(ret);
            
            ret = acbrETQ.ETQ_ConfigGravarValor(ACBrETQ.toUTF8("Principal"), ACBrETQ.toUTF8("LogPath"), pathLog.toString());
            ACBrETQ.checkResult(ret);
            
            ret = acbrETQ.ETQ_ConfigGravar(ACBrETQ.toUTF8(""));
            ACBrETQ.checkResult(ret);
            
            loadConfig();
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(this, ex);
        }
    }//GEN-LAST:event_formWindowActivated

    private void btnCarregarImagemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnCarregarImagemActionPerformed
        try {
            JFileChooser fc = new JFileChooser();
            OpenFileFilter filter = new OpenFileFilter("bmp|img|pcx", "BMP MonoCromático (*.bmp, *.img, *.pcx)");
            fc.addChoosableFileFilter(filter);
            fc.setFileFilter(filter);
            int result = fc.showOpenDialog(this);
            if (result == JFileChooser.APPROVE_OPTION) {
                File file = fc.getSelectedFile();
                BufferedImage myPicture = ImageIO.read(file);
                JLabel picLabel = new JLabel(new ImageIcon(myPicture));
                pnlImagem.add(picLabel);                
                pnlImagem.revalidate(); 
                pnlImagem.repaint();
                
                int ret = acbrETQ.ETQ_CarregarImagem(file.getAbsolutePath(), txtImagem.getText(), false);
                ACBrETQ.checkResult(ret);
            }
        } catch (Exception ex) {
            Logger.getLogger(FrmMain.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_btnCarregarImagemActionPerformed

    private void btnEtqSimplesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnEtqSimplesActionPerformed
        try {
            saveConfig();
            
            int ret = acbrETQ.ETQ_Ativar();
            ACBrETQ.checkResult(ret);
            
            if(Arrays.asList(1, 2).contains(cmbModelo.getSelectedIndex())){
                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirCaixa(13, 32, 56, 17, 1, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "20,59", 0, false);
                ACBrETQ.checkResult(ret);                
            }else{
                ret = acbrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(2, "S", 10, 10, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirCaixa(13, 32, 56, 17, 1, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "20,59", 0, false);
                ACBrETQ.checkResult(ret);
            }
            
            ret = acbrETQ.ETQ_Imprimir((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
            ACBrETQ.checkResult(ret);

            ret = acbrETQ.ETQ_Desativar();
            ACBrETQ.checkResult(ret);
        } catch (Exception ex) {
            Logger.getLogger(FrmMain.class.getName()).log(Level.SEVERE, null, ex);
            JOptionPane.showMessageDialog(this, ex);
        }
    }//GEN-LAST:event_btnEtqSimplesActionPerformed

    private void btnEtq3ColunasActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnEtq3ColunasActionPerformed
        try {
            saveConfig();
            
            int ret = acbrETQ.ETQ_Ativar();
            ACBrETQ.checkResult(ret);
            
            if(Arrays.asList(1, 2).contains(cmbModelo.getSelectedIndex())){
                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 3, "BISCOITO REC 33G", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685", 10, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 32, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 32, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685", 10, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 61, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 61, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685", 10, 0);
                ACBrETQ.checkResult(ret);
            }else{
                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "0", 20, 30, 2, 3, "BISCOITO REC 33G", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "0", 20, 20, 6, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685", 10, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "0", 20, 30, 2, 32, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "0", 20, 20, 6, 32, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685", 10, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "0", 20, 30, 2, 61, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "0", 20, 20, 6, 61, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685", 10, 0);
                ACBrETQ.checkResult(ret);
            }
            
            ret = acbrETQ.ETQ_Imprimir((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
            ACBrETQ.checkResult(ret);
            
            ret = acbrETQ.ETQ_Desativar();
            ACBrETQ.checkResult(ret);
        } catch (Exception ex) {
            Logger.getLogger(FrmMain.class.getName()).log(Level.SEVERE, null, ex);
            JOptionPane.showMessageDialog(this, ex);
        }
    }//GEN-LAST:event_btnEtq3ColunasActionPerformed

    private void btnEtqBlocoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnEtqBlocoActionPerformed
        try {
            saveConfig();
            
            int ret = acbrETQ.ETQ_Ativar();
            ACBrETQ.checkResult(ret);
            
            if(Arrays.asList(1, 2).contains(cmbModelo.getSelectedIndex())){
                ret = acbrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "20,59", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_FinalizarEtiqueta((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "SABAO EM PO FLASH 1KG", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "ADVANCED - UNIDADE", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898903097042", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "3,18", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_FinalizarEtiqueta((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "MACIO MATRIX FIX", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898237690230", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "8,60", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_FinalizarEtiqueta((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
                ACBrETQ.checkResult(ret);
            }else{
                ret = acbrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "S", 10, 10, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "20,59", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_FinalizarEtiqueta((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "SABAO EM PO FLASH 1KG", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "S", 10, 10, 8, 3, "ADVANCED - UNIDADE", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898903097042", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "3,18", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_FinalizarEtiqueta((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, true);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "S", 10, 10, 8, 3, "MACIO MATRIX FIX", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898237690230", 10, 1);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "8,60", 0, false);
                ACBrETQ.checkResult(ret);

                ret = acbrETQ.ETQ_FinalizarEtiqueta((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
                ACBrETQ.checkResult(ret);
            }
            
            ret = acbrETQ.ETQ_Imprimir(1, (int)nudAvancoEtq.getValue());
            ACBrETQ.checkResult(ret);
            
            ret = acbrETQ.ETQ_Desativar();
            ACBrETQ.checkResult(ret);
        } catch (Exception ex) {
            Logger.getLogger(FrmMain.class.getName()).log(Level.SEVERE, null, ex);
            JOptionPane.showMessageDialog(this, ex);
        }
    }//GEN-LAST:event_btnEtqBlocoActionPerformed

    private void btnEtqImagemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnEtqImagemActionPerformed
        try {
            saveConfig();

            int ret = acbrETQ.ETQ_Ativar();
            ACBrETQ.checkResult(ret);

            ret = acbrETQ.ETQ_ImprimirImagem(1, 10, 10, txtImagem.getText());
            ACBrETQ.checkResult(ret);

            ret = acbrETQ.ETQ_Imprimir((int)nudCopias.getValue(), (int)nudAvancoEtq.getValue());
            ACBrETQ.checkResult(ret);

            ret = acbrETQ.ETQ_Desativar();
            ACBrETQ.checkResult(ret);
        } catch (Exception ex) {
            Logger.getLogger(FrmMain.class.getName()).log(Level.SEVERE, null, ex);
            JOptionPane.showMessageDialog(this, ex);
        }
    }//GEN-LAST:event_btnEtqImagemActionPerformed
   
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnCarregarImagem;
    private javax.swing.JButton btnEtq3Colunas;
    private javax.swing.JButton btnEtqBloco;
    private javax.swing.JButton btnEtqImagem;
    private javax.swing.JButton btnEtqSimples;
    private javax.swing.JCheckBox chkLimparMemoria;
    private javax.swing.JComboBox cmbBackFeed;
    private javax.swing.JComboBox cmbDPi;
    private javax.swing.JComboBox cmbModelo;
    private javax.swing.JComboBox<String> cmbPorta;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JSpinner nudAvancoEtq;
    private javax.swing.JSpinner nudCopias;
    private javax.swing.JSpinner nudTemperatura;
    private javax.swing.JSpinner nudVelocidade;
    private javax.swing.JPanel pnlImagem;
    private javax.swing.JTextField txtImagem;
    // End of variables declaration//GEN-END:variables
}
