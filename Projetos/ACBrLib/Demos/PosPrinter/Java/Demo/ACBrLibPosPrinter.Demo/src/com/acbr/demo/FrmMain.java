/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.demo;

import com.acbr.posprinter.*;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 *
 * @author rften
 */
public class FrmMain extends javax.swing.JFrame {

    private ACBrPosPrinter posPrinter;

    /**
     * Creates new form FrmMain
     */
    public FrmMain() {
        initComponents();
    }

    private void toogleActive() throws Exception {
        if ("Ativar".equals(btnAtivar.getText())) {
            saveConfig();
            int ret = posPrinter.POS_Ativar();
            ACBrPosPrinter.checkResult(ret);
            btnAtivar.setText("Desativar");
        } else {
            int ret = posPrinter.POS_Desativar();
            ACBrPosPrinter.checkResult(ret);
            btnAtivar.setText("Ativar");
        }
    }

    private void loadConfig() throws Exception {
        int ret;
        ByteBuffer buffer;
        IntByReference bufferLen;

        ret = posPrinter.POS_ConfigLer(ACBrPosPrinter.toUTF8(""));
        ACBrPosPrinter.checkResult(ret);

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("Modelo"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        int modelo = Integer.parseInt(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue()));
        cmbModelo.setSelectedItem(ACBrPosPrinterModelo.valueOf(modelo));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("Porta"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        cmbPorta.setSelectedItem(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue()));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("ColunasFonteNormal"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        spnColunas.setValue(Integer.parseInt(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("EspacoEntreLinhas"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        spnEspacos.setValue(Integer.parseInt(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("LinhasBuffer"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        spnBuffer.setValue(Integer.parseInt(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("LinhasEntreCupons"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        spnLinhasPular.setValue(Integer.parseInt(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("ControlePorta"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        cbxControlePorta.setSelected("1".equals(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("CortaPapel"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        cbxCortarPapel.setSelected("1".equals(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("TraduzirTags"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        cbxTraduzirTags.setSelected("1".equals(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("IgnorarTags"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        cbxIgnorarTags.setSelected("1".equals(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue())));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("ArqLog"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        txtArqLog.setText(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue()));

        buffer = ByteBuffer.allocate(256);
        bufferLen = new IntByReference(256);

        ret = posPrinter.POS_ConfigLerValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("PaginaDeCodigo"), buffer, bufferLen);
        ACBrPosPrinter.checkResult(ret);

        int codePage = Integer.parseInt(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue()));
        cmbCodePage.setSelectedItem(PosPaginaCodigo.valueOf(codePage));
    }

    private void saveConfig() throws Exception {
        int ret;

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("Modelo"),
                Integer.toString(((ACBrPosPrinterModelo) cmbModelo.getSelectedItem()).asInt()));
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("Porta"), cmbPorta.getSelectedItem().toString());
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("ColunasFonteNormal"), spnColunas.getValue().toString());
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("EspacoEntreLinhas"), spnEspacos.getValue().toString());
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("LinhasBuffer"), spnBuffer.getValue().toString());
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("LinhasEntreCupons"), spnLinhasPular.getValue().toString());
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("ControlePorta"), cbxControlePorta.isSelected() ? "1" : "0");
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("CortaPapel"), cbxCortarPapel.isSelected() ? "1" : "0");
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("TraduzirTags"), cbxTraduzirTags.isSelected() ? "1" : "0");
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("IgnorarTags"), cbxIgnorarTags.isSelected() ? "1" : "0");
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("ArqLog"), txtArqLog.getText());
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("PosPrinter"), ACBrPosPrinter.toUTF8("PaginaDeCodigo"),
                Integer.toString(((PosPaginaCodigo) cmbCodePage.getSelectedItem()).asInt()));
        ACBrPosPrinter.checkResult(ret);

        ret = posPrinter.POS_ConfigGravar(ACBrPosPrinter.toUTF8(""));
        ACBrPosPrinter.checkResult(ret);
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
        cmbPorta = new javax.swing.JComboBox<>();
        jLabel1 = new javax.swing.JLabel();
        btnAtivar = new javax.swing.JButton();
        cmbModelo = new javax.swing.JComboBox<>();
        jLabel2 = new javax.swing.JLabel();
        spnColunas = new javax.swing.JSpinner();
        jLabel3 = new javax.swing.JLabel();
        spnEspacos = new javax.swing.JSpinner();
        spnBuffer = new javax.swing.JSpinner();
        spnLinhasPular = new javax.swing.JSpinner();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        cbxControlePorta = new javax.swing.JCheckBox();
        cbxCortarPapel = new javax.swing.JCheckBox();
        cbxTraduzirTags = new javax.swing.JCheckBox();
        cbxIgnorarTags = new javax.swing.JCheckBox();
        jLabel7 = new javax.swing.JLabel();
        txtArqLog = new javax.swing.JTextField();
        btnArqLog = new javax.swing.JButton();
        jLabel8 = new javax.swing.JLabel();
        cmbCodePage = new javax.swing.JComboBox<>();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jScrollPane1 = new javax.swing.JScrollPane();
        txtImprimir = new javax.swing.JTextArea();
        jPanel2 = new javax.swing.JPanel();
        btnImprimir = new javax.swing.JButton();
        btnLimpar = new javax.swing.JButton();
        btnAddTags = new javax.swing.JButton();
        btnInfoImp = new javax.swing.JButton();
        btnStatusImp = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Demo ACBrLibPosPrinter Java");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowOpened(java.awt.event.WindowEvent evt) {
                formWindowOpened(evt);
            }
        });

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Configurações"));

        cmbPorta.setEditable(true);
        cmbPorta.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "COM1", "COM2", "LPT1", "LPT2", "\\\\localhost\\Epson", "c:\\temp\\ecf.txt", "TCP:192.168.0.31:9100" }));
        cmbPorta.setSelectedItem("c:\\temp\\ecf.txt");
        cmbPorta.setToolTipText("");

        jLabel1.setText("Modelo");

        btnAtivar.setIcon(new javax.swing.ImageIcon(getClass().getResource("/resources/images/ativar.png"))); // NOI18N
        btnAtivar.setText("Ativar");
        btnAtivar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAtivarActionPerformed(evt);
            }
        });

        cmbModelo.setModel(new DefaultComboBoxModel<>(ACBrPosPrinterModelo.values()));
        cmbModelo.setToolTipText("");

        jLabel2.setText("Porta");

        jLabel3.setText("Colunas");

        jLabel4.setText("Espaços");

        jLabel5.setText("Buffer");

        jLabel6.setText("Linhas Pular");

        cbxControlePorta.setText("Controle Porta");

        cbxCortarPapel.setSelected(true);
        cbxCortarPapel.setText("Cortar Papel");

        cbxTraduzirTags.setSelected(true);
        cbxTraduzirTags.setText("Traduzir Tags");

        cbxIgnorarTags.setText("Ignorar Tags");

        jLabel7.setText("Arq. Log");

        txtArqLog.setText("PosPrinter.log");
        txtArqLog.setToolTipText("");

        btnArqLog.setText("...");
        btnArqLog.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnArqLogActionPerformed(evt);
            }
        });

        jLabel8.setText("Code Page");

        cmbCodePage.setModel(new DefaultComboBoxModel<>(PosPaginaCodigo.values()));
        cmbCodePage.setSelectedItem(PosPaginaCodigo.pc850);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(spnColunas, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabel3))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanel1Layout.createSequentialGroup()
                                        .addComponent(jLabel4)
                                        .addGap(24, 24, 24))
                                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                        .addComponent(spnEspacos, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED))))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(cbxControlePorta)
                                    .addComponent(cbxCortarPapel)
                                    .addComponent(cbxTraduzirTags)
                                    .addComponent(cbxIgnorarTags))
                                .addGap(24, 24, 24)))
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel8)
                                    .addComponent(cmbCodePage, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel1Layout.createSequentialGroup()
                                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                            .addComponent(spnBuffer, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                                            .addComponent(jLabel5))
                                        .addGap(6, 6, 6)
                                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                            .addComponent(jLabel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                            .addComponent(spnLinhasPular)))
                                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel1Layout.createSequentialGroup()
                                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                            .addComponent(jLabel7)
                                            .addGroup(jPanel1Layout.createSequentialGroup()
                                                .addComponent(txtArqLog, javax.swing.GroupLayout.PREFERRED_SIZE, 91, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(btnArqLog, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE)))
                                        .addGap(0, 0, Short.MAX_VALUE))
                                    .addComponent(btnAtivar, javax.swing.GroupLayout.PREFERRED_SIZE, 114, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addGap(60, 60, 60))))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2)
                            .addComponent(cmbModelo, javax.swing.GroupLayout.PREFERRED_SIZE, 118, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(cmbPorta, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addGap(60, 60, 60))))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGap(4, 4, 4)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cmbModelo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jLabel2))
                    .addComponent(btnAtivar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(cmbPorta, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(3, 3, 3)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel5)
                        .addComponent(jLabel6))
                    .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel3)
                        .addComponent(jLabel4)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(spnLinhasPular, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(spnBuffer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(spnColunas, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(spnEspacos, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addGap(18, 18, 18)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cbxControlePorta)
                    .addComponent(jLabel7))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cbxCortarPapel)
                    .addComponent(txtArqLog, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnArqLog))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cbxTraduzirTags)
                    .addComponent(jLabel8))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cbxIgnorarTags)
                    .addComponent(cmbCodePage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel1.getAccessibleContext().setAccessibleDescription("");

        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        jScrollPane1.setAutoscrolls(true);

        txtImprimir.setColumns(20);
        txtImprimir.setRows(5);
        jScrollPane1.setViewportView(txtImprimir);

        jTabbedPane1.addTab("Texto a Imprimir", jScrollPane1);

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        btnImprimir.setIcon(new javax.swing.ImageIcon(getClass().getResource("/resources/images/imprimir.png"))); // NOI18N
        btnImprimir.setText("Imprimir");
        btnImprimir.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnImprimirActionPerformed(evt);
            }
        });

        btnLimpar.setIcon(new javax.swing.ImageIcon(getClass().getResource("/resources/images/limpar.png"))); // NOI18N
        btnLimpar.setText("Limpar");
        btnLimpar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnLimparActionPerformed(evt);
            }
        });

        btnAddTags.setText("Add. Tags");
        btnAddTags.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAddTagsActionPerformed(evt);
            }
        });

        btnInfoImp.setText("Info Imp.");
        btnInfoImp.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnInfoImpActionPerformed(evt);
            }
        });

        btnStatusImp.setText("Status Imp.");
        btnStatusImp.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnStatusImpActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(btnImprimir, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnLimpar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnAddTags, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnStatusImp, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnInfoImp, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btnAddTags)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnInfoImp)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnStatusImp)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(btnLimpar)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnImprimir)
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, 268, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTabbedPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 428, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                    .addComponent(jTabbedPane1, javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void btnAddTagsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAddTagsActionPerformed
        txtImprimir.setText("");
        txtImprimir.append("</zera>" + System.lineSeparator());
        txtImprimir.append("</linha_dupla>" + System.lineSeparator());
        //txtImprimir.append(String.format("FONTE NORMAL: {(int)nudColunas.Value} Colunas",  ,System.lineSeparator));
        txtImprimir.append("</c><n>FONTE NEGRITO</N>" + System.lineSeparator());
        txtImprimir.append("<in>FONTE INVERTIDA</in>" + System.lineSeparator());
        txtImprimir.append("<S>FONTE SUBLINHADA</s>" + System.lineSeparator());
        txtImprimir.append("<i>FONTE ITALICO</i>" + System.lineSeparator());
        txtImprimir.append("FONTE NORMAL" + System.lineSeparator());
        txtImprimir.append("</linha_simples>" + System.lineSeparator());
        txtImprimir.append("<n>LIGA NEGRITO" + System.lineSeparator());
        txtImprimir.append("<i>LIGA ITALICO" + System.lineSeparator());
        txtImprimir.append("<S>LIGA SUBLINHADA" + System.lineSeparator());
        txtImprimir.append("<c>LIGA CONDENSADA" + System.lineSeparator());
        txtImprimir.append("<e>LIGA EXPANDIDA" + System.lineSeparator());
        txtImprimir.append("<a>LIGA ALTURA DUPLA" + System.lineSeparator());
        txtImprimir.append("</fn>FONTE NORMAL" + System.lineSeparator());
        txtImprimir.append("</linha_simples>" + System.lineSeparator());
        txtImprimir.append("<e><n>NEGRITO E EXPANDIDA</n></e>" + System.lineSeparator());
        txtImprimir.append("<c><n>NEGRITO E CONDENSADA</n></c>" + System.lineSeparator());
        txtImprimir.append("<e><a>EXPANDIDA E ALT.DUPLA</a></e>" + System.lineSeparator());
        txtImprimir.append("</fn>FONTE NORMAL" + System.lineSeparator());
        txtImprimir.append("<in><e>INVERTIDA E EXPANDIDA</e></in>" + System.lineSeparator());
        txtImprimir.append("<in><c>INVERTIDA E CONDENSADA</c></in>" + System.lineSeparator());
        txtImprimir.append("<in><a>INVERTIDA E ALT.DUPLA</a></in>" + System.lineSeparator());
        txtImprimir.append("</fn>FONTE NORMAL" + System.lineSeparator());
        txtImprimir.append("</linha_simples>" + System.lineSeparator());
        txtImprimir.append("</FB>FONTE TIPO B" + System.lineSeparator());
        txtImprimir.append("<n>FONTE NEGRITO</N>" + System.lineSeparator());
        txtImprimir.append("<e>FONTE EXPANDIDA</e>" + System.lineSeparator());
        txtImprimir.append("<a>FONTE ALT.DUPLA</a>" + System.lineSeparator());
        txtImprimir.append("<in>FONTE INVERTIDA</in>" + System.lineSeparator());
        txtImprimir.append("<S>FONTE SUBLINHADA</s>" + System.lineSeparator());
        txtImprimir.append("<i>FONTE ITALICO</i>" + System.lineSeparator());
        txtImprimir.append("</FA>FONTE TIPO A" + System.lineSeparator());
        txtImprimir.append("</FN>FONTE NORMAL" + System.lineSeparator());
        txtImprimir.append("</corte_total>" + System.lineSeparator());
        txtImprimir.append("<c>CODE128C: 35150711111111111111591234567890001135408700</c>" + System.lineSeparator());
        txtImprimir.append("<code128c>35150711111111111111591234567890001135408700</code128c>" + System.lineSeparator());
        txtImprimir.append("</Linha_Simples>" + System.lineSeparator());
        txtImprimir.append("UPCA: 12345678901" + System.lineSeparator());
        txtImprimir.append("<upca>12345678901</upca>" + System.lineSeparator());
        txtImprimir.append("</Linha_Simples>" + System.lineSeparator());
        txtImprimir.append("CODABAR: A123456789012345A" + System.lineSeparator());
        txtImprimir.append("<codabar>A123456789012345A</codabar>" + System.lineSeparator());
        txtImprimir.append("</Linha_Simples>" + System.lineSeparator());
        txtImprimir.append("MSI: 1234567890" + System.lineSeparator());
        txtImprimir.append("<msi>1234567890</msi>" + System.lineSeparator());
        txtImprimir.append("</corte_total>" + System.lineSeparator());
    }//GEN-LAST:event_btnAddTagsActionPerformed

    private void btnLimparActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLimparActionPerformed
        txtImprimir.setText("");
    }//GEN-LAST:event_btnLimparActionPerformed

    private void btnArqLogActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnArqLogActionPerformed
        JFileChooser chooser = new JFileChooser();
        OpenFileFilter filter = new OpenFileFilter("log", "Log File (*.log)");
        chooser.addChoosableFileFilter(filter);
        chooser.setFileFilter(filter);

        int returnVal = chooser.showSaveDialog(rootPane);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            txtArqLog.setText(chooser.getSelectedFile().getPath());
        }
    }//GEN-LAST:event_btnArqLogActionPerformed

    private void btnInfoImpActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnInfoImpActionPerformed
        try {
            ByteBuffer buffer = ByteBuffer.allocate(256);
            IntByReference bufferLen = new IntByReference(256);

            int ret = posPrinter.POS_LerInfoImpressora(buffer, bufferLen);
            ACBrPosPrinter.checkResult(ret);

            txtImprimir.setText(ACBrPosPrinter.fromUTF8(buffer, bufferLen.getValue()));
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(rootPane, ex);
        }
    }//GEN-LAST:event_btnInfoImpActionPerformed

    private void btnStatusImpActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnStatusImpActionPerformed
        try {
            IntByReference intStatus = new IntByReference(256);

            int ret = posPrinter.POS_LerStatusImpressora(1, intStatus);
            ACBrPosPrinter.checkResult(ret);

            Set<ACBrPosTipoStatus> statusSet = ACBrPosTipoStatus.valueOf(intStatus.getValue());
            txtImprimir.setText("");
            for (ACBrPosTipoStatus status : statusSet) {
                txtImprimir.append("[" + status.name() + "]" + System.lineSeparator());
            }
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(rootPane, ex);
        }
    }//GEN-LAST:event_btnStatusImpActionPerformed

    private void btnImprimirActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnImprimirActionPerformed
        try {
            int ret = posPrinter.POS_Imprimir(ACBrPosPrinter.toUTF8(txtImprimir.getText()), false, true, true, 1);
            ACBrPosPrinter.checkResult(ret);

        } catch (Exception ex) {
            JOptionPane.showMessageDialog(rootPane, ex);
        }
    }//GEN-LAST:event_btnImprimirActionPerformed

    private void formWindowOpened(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowOpened
        try {
            posPrinter = ACBrPosPrinter.INSTANCE;
            File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
            if(!iniFile.exists()) iniFile.createNewFile();
            
            int ret = posPrinter.POS_Inicializar(ACBrPosPrinter.toUTF8(iniFile.getAbsolutePath()), ACBrPosPrinter.toUTF8(""));
            ACBrPosPrinter.checkResult(ret);

            loadConfig();

            Path pathLog = Paths.get(System.getProperty("user.dir"), "Docs");
            if (!Files.isDirectory(pathLog)) {
                pathLog.toFile().mkdirs();
            }

            ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("Principal"), ACBrPosPrinter.toUTF8("LogNivel"), "4");
            ACBrPosPrinter.checkResult(ret);
            
            ret = posPrinter.POS_ConfigGravarValor(ACBrPosPrinter.toUTF8("Principal"), ACBrPosPrinter.toUTF8("LogPath"), pathLog.toString());
            ACBrPosPrinter.checkResult(ret);
            
            ret = posPrinter.POS_ConfigGravar(ACBrPosPrinter.toUTF8(""));
            ACBrPosPrinter.checkResult(ret);
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(rootPane, ex);
        }
    }//GEN-LAST:event_formWindowOpened

    private void btnAtivarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAtivarActionPerformed
        try {
            toogleActive();
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(rootPane, ex);
        }
    }//GEN-LAST:event_btnAtivarActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnAddTags;
    private javax.swing.JButton btnArqLog;
    private javax.swing.JButton btnAtivar;
    private javax.swing.JButton btnImprimir;
    private javax.swing.JButton btnInfoImp;
    private javax.swing.JButton btnLimpar;
    private javax.swing.JButton btnStatusImp;
    private javax.swing.JCheckBox cbxControlePorta;
    private javax.swing.JCheckBox cbxCortarPapel;
    private javax.swing.JCheckBox cbxIgnorarTags;
    private javax.swing.JCheckBox cbxTraduzirTags;
    private javax.swing.JComboBox<PosPaginaCodigo> cmbCodePage;
    private javax.swing.JComboBox<ACBrPosPrinterModelo> cmbModelo;
    private javax.swing.JComboBox<String> cmbPorta;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JSpinner spnBuffer;
    private javax.swing.JSpinner spnColunas;
    private javax.swing.JSpinner spnEspacos;
    private javax.swing.JSpinner spnLinhasPular;
    private javax.swing.JTextField txtArqLog;
    private javax.swing.JTextArea txtImprimir;
    // End of variables declaration//GEN-END:variables
}
