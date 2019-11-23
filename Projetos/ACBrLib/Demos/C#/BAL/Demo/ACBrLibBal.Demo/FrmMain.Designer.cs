namespace ACBrLibBal.Demo
{
    partial class FrmMain
    {
        /// <summary>
        /// Variável de designer necessária.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Limpar os recursos que estão sendo usados.
        /// </summary>
        /// <param name="disposing">true se for necessário descartar os recursos gerenciados; caso contrário, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Código gerado pelo Windows Form Designer

        /// <summary>
        /// Método necessário para suporte ao Designer - não modifique 
        /// o conteúdo deste método com o editor de código.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FrmMain));
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.btnAtivar = new System.Windows.Forms.Button();
            this.btnLerPeso = new System.Windows.Forms.Button();
            this.btnSolicitarPeso = new System.Windows.Forms.Button();
            this.btnUlitmoPeso = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.txtPesoLido = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.cmbModelo = new System.Windows.Forms.ComboBox();
            this.label9 = new System.Windows.Forms.Label();
            this.cmbPorta = new System.Windows.Forms.ComboBox();
            this.chkSoftFlow = new System.Windows.Forms.CheckBox();
            this.chkHardFlow = new System.Windows.Forms.CheckBox();
            this.label4 = new System.Windows.Forms.Label();
            this.cmbHandshaking = new System.Windows.Forms.ComboBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.cmbStopbits = new System.Windows.Forms.ComboBox();
            this.cmbParity = new System.Windows.Forms.ComboBox();
            this.cmbDatabits = new System.Windows.Forms.ComboBox();
            this.cmbBaud = new System.Windows.Forms.ComboBox();
            this.velocidadeLabel = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.nudMaxBand = new System.Windows.Forms.NumericUpDown();
            this.label10 = new System.Windows.Forms.Label();
            this.nudBytesCount = new System.Windows.Forms.NumericUpDown();
            this.nudIntervalo = new System.Windows.Forms.NumericUpDown();
            this.label11 = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudMaxBand)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudBytesCount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudIntervalo)).BeginInit();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.nudIntervalo);
            this.groupBox1.Controls.Add(this.label11);
            this.groupBox1.Controls.Add(this.nudBytesCount);
            this.groupBox1.Controls.Add(this.label10);
            this.groupBox1.Controls.Add(this.nudMaxBand);
            this.groupBox1.Controls.Add(this.label7);
            this.groupBox1.Controls.Add(this.chkSoftFlow);
            this.groupBox1.Controls.Add(this.chkHardFlow);
            this.groupBox1.Controls.Add(this.label4);
            this.groupBox1.Controls.Add(this.cmbHandshaking);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.label5);
            this.groupBox1.Controls.Add(this.label6);
            this.groupBox1.Controls.Add(this.cmbStopbits);
            this.groupBox1.Controls.Add(this.cmbParity);
            this.groupBox1.Controls.Add(this.cmbDatabits);
            this.groupBox1.Controls.Add(this.cmbBaud);
            this.groupBox1.Controls.Add(this.velocidadeLabel);
            this.groupBox1.Controls.Add(this.label9);
            this.groupBox1.Controls.Add(this.cmbPorta);
            this.groupBox1.Controls.Add(this.cmbModelo);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(258, 275);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Configurações";
            // 
            // btnAtivar
            // 
            this.btnAtivar.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnAtivar.Location = new System.Drawing.Point(276, 18);
            this.btnAtivar.Name = "btnAtivar";
            this.btnAtivar.Size = new System.Drawing.Size(128, 26);
            this.btnAtivar.TabIndex = 1;
            this.btnAtivar.Text = "Ativar";
            this.btnAtivar.UseVisualStyleBackColor = true;
            this.btnAtivar.Click += new System.EventHandler(this.btnAtivar_Click);
            // 
            // btnLerPeso
            // 
            this.btnLerPeso.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnLerPeso.Location = new System.Drawing.Point(276, 101);
            this.btnLerPeso.Name = "btnLerPeso";
            this.btnLerPeso.Size = new System.Drawing.Size(128, 26);
            this.btnLerPeso.TabIndex = 2;
            this.btnLerPeso.Text = "Ler Peso";
            this.btnLerPeso.UseVisualStyleBackColor = true;
            this.btnLerPeso.Click += new System.EventHandler(this.btnLerPeso_Click);
            // 
            // btnSolicitarPeso
            // 
            this.btnSolicitarPeso.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnSolicitarPeso.Location = new System.Drawing.Point(276, 133);
            this.btnSolicitarPeso.Name = "btnSolicitarPeso";
            this.btnSolicitarPeso.Size = new System.Drawing.Size(128, 26);
            this.btnSolicitarPeso.TabIndex = 3;
            this.btnSolicitarPeso.Text = "Solicitar Peso";
            this.btnSolicitarPeso.UseVisualStyleBackColor = true;
            this.btnSolicitarPeso.Click += new System.EventHandler(this.btnSolicitarPeso_Click);
            // 
            // btnUlitmoPeso
            // 
            this.btnUlitmoPeso.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnUlitmoPeso.Location = new System.Drawing.Point(276, 165);
            this.btnUlitmoPeso.Name = "btnUlitmoPeso";
            this.btnUlitmoPeso.Size = new System.Drawing.Size(128, 26);
            this.btnUlitmoPeso.TabIndex = 4;
            this.btnUlitmoPeso.Text = "Ultimo Peso Lido";
            this.btnUlitmoPeso.UseVisualStyleBackColor = true;
            this.btnUlitmoPeso.Click += new System.EventHandler(this.btnUlitmoPeso_Click);
            // 
            // label1
            // 
            this.label1.Font = new System.Drawing.Font("Tahoma", 15.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(276, 226);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(128, 23);
            this.label1.TabIndex = 5;
            this.label1.Text = "Peso Lido";
            this.label1.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // txtPesoLido
            // 
            this.txtPesoLido.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
            this.txtPesoLido.Font = new System.Drawing.Font("Tahoma", 15.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtPesoLido.Location = new System.Drawing.Point(276, 254);
            this.txtPesoLido.Name = "txtPesoLido";
            this.txtPesoLido.ReadOnly = true;
            this.txtPesoLido.Size = new System.Drawing.Size(128, 33);
            this.txtPesoLido.TabIndex = 6;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 19);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(48, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "Modelo";
            // 
            // cmbModelo
            // 
            this.cmbModelo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbModelo.FormattingEnabled = true;
            this.cmbModelo.Location = new System.Drawing.Point(6, 35);
            this.cmbModelo.Name = "cmbModelo";
            this.cmbModelo.Size = new System.Drawing.Size(235, 21);
            this.cmbModelo.TabIndex = 1;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(3, 59);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(37, 13);
            this.label9.TabIndex = 16;
            this.label9.Text = "Porta";
            // 
            // cmbPorta
            // 
            this.cmbPorta.FormattingEnabled = true;
            this.cmbPorta.Location = new System.Drawing.Point(6, 75);
            this.cmbPorta.Name = "cmbPorta";
            this.cmbPorta.Size = new System.Drawing.Size(235, 21);
            this.cmbPorta.TabIndex = 15;
            // 
            // chkSoftFlow
            // 
            this.chkSoftFlow.AutoSize = true;
            this.chkSoftFlow.Location = new System.Drawing.Point(121, 227);
            this.chkSoftFlow.Name = "chkSoftFlow";
            this.chkSoftFlow.Size = new System.Drawing.Size(75, 17);
            this.chkSoftFlow.TabIndex = 42;
            this.chkSoftFlow.Text = "SoftFlow";
            this.chkSoftFlow.UseVisualStyleBackColor = true;
            // 
            // chkHardFlow
            // 
            this.chkHardFlow.AutoSize = true;
            this.chkHardFlow.Location = new System.Drawing.Point(121, 250);
            this.chkHardFlow.Name = "chkHardFlow";
            this.chkHardFlow.Size = new System.Drawing.Size(79, 17);
            this.chkHardFlow.TabIndex = 41;
            this.chkHardFlow.Text = "HardFlow";
            this.chkHardFlow.UseVisualStyleBackColor = true;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(3, 224);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(81, 13);
            this.label4.TabIndex = 40;
            this.label4.Text = "&Handshaking";
            // 
            // cmbHandshaking
            // 
            this.cmbHandshaking.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbHandshaking.FormattingEnabled = true;
            this.cmbHandshaking.Location = new System.Drawing.Point(6, 240);
            this.cmbHandshaking.Name = "cmbHandshaking";
            this.cmbHandshaking.Size = new System.Drawing.Size(109, 21);
            this.cmbHandshaking.TabIndex = 39;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(129, 145);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(58, 13);
            this.label3.TabIndex = 38;
            this.label3.Text = "&Stop Bits";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(3, 145);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(43, 13);
            this.label5.TabIndex = 37;
            this.label5.Text = "&Parity ";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(129, 105);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(59, 13);
            this.label6.TabIndex = 36;
            this.label6.Text = "&Data Bits";
            // 
            // cmbStopbits
            // 
            this.cmbStopbits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbStopbits.FormattingEnabled = true;
            this.cmbStopbits.Location = new System.Drawing.Point(132, 161);
            this.cmbStopbits.Name = "cmbStopbits";
            this.cmbStopbits.Size = new System.Drawing.Size(109, 21);
            this.cmbStopbits.TabIndex = 35;
            // 
            // cmbParity
            // 
            this.cmbParity.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbParity.FormattingEnabled = true;
            this.cmbParity.Location = new System.Drawing.Point(6, 161);
            this.cmbParity.Name = "cmbParity";
            this.cmbParity.Size = new System.Drawing.Size(109, 21);
            this.cmbParity.TabIndex = 34;
            // 
            // cmbDatabits
            // 
            this.cmbDatabits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbDatabits.FormattingEnabled = true;
            this.cmbDatabits.Items.AddRange(new object[] {
            "5",
            "6",
            "7",
            "8"});
            this.cmbDatabits.Location = new System.Drawing.Point(132, 121);
            this.cmbDatabits.Name = "cmbDatabits";
            this.cmbDatabits.Size = new System.Drawing.Size(109, 21);
            this.cmbDatabits.TabIndex = 33;
            // 
            // cmbBaud
            // 
            this.cmbBaud.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbBaud.FormattingEnabled = true;
            this.cmbBaud.Location = new System.Drawing.Point(6, 121);
            this.cmbBaud.Name = "cmbBaud";
            this.cmbBaud.Size = new System.Drawing.Size(109, 21);
            this.cmbBaud.TabIndex = 32;
            // 
            // velocidadeLabel
            // 
            this.velocidadeLabel.AutoSize = true;
            this.velocidadeLabel.Location = new System.Drawing.Point(3, 105);
            this.velocidadeLabel.Name = "velocidadeLabel";
            this.velocidadeLabel.Size = new System.Drawing.Size(36, 13);
            this.velocidadeLabel.TabIndex = 31;
            this.velocidadeLabel.Text = "Baud";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(3, 185);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(71, 13);
            this.label7.TabIndex = 43;
            this.label7.Text = "Max. Band.";
            // 
            // nudMaxBand
            // 
            this.nudMaxBand.Location = new System.Drawing.Point(6, 201);
            this.nudMaxBand.Name = "nudMaxBand";
            this.nudMaxBand.Size = new System.Drawing.Size(68, 20);
            this.nudMaxBand.TabIndex = 44;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(77, 185);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(75, 13);
            this.label10.TabIndex = 45;
            this.label10.Text = "Bytes Count";
            // 
            // nudBytesCount
            // 
            this.nudBytesCount.Location = new System.Drawing.Point(80, 201);
            this.nudBytesCount.Name = "nudBytesCount";
            this.nudBytesCount.Size = new System.Drawing.Size(75, 20);
            this.nudBytesCount.TabIndex = 46;
            // 
            // nudIntervalo
            // 
            this.nudIntervalo.Location = new System.Drawing.Point(160, 201);
            this.nudIntervalo.Name = "nudIntervalo";
            this.nudIntervalo.Size = new System.Drawing.Size(81, 20);
            this.nudIntervalo.TabIndex = 48;
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(157, 185);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(57, 13);
            this.label11.TabIndex = 47;
            this.label11.Text = "Intervalo";
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(415, 299);
            this.Controls.Add(this.txtPesoLido);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.btnUlitmoPeso);
            this.Controls.Add(this.btnSolicitarPeso);
            this.Controls.Add(this.btnLerPeso);
            this.Controls.Add(this.btnAtivar);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibBAL Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudMaxBand)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudBytesCount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudIntervalo)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button btnAtivar;
        private System.Windows.Forms.Button btnLerPeso;
        private System.Windows.Forms.Button btnSolicitarPeso;
        private System.Windows.Forms.Button btnUlitmoPeso;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox txtPesoLido;
        private System.Windows.Forms.ComboBox cmbModelo;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.NumericUpDown nudIntervalo;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.NumericUpDown nudBytesCount;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.NumericUpDown nudMaxBand;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.CheckBox chkSoftFlow;
        private System.Windows.Forms.CheckBox chkHardFlow;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.ComboBox cmbHandshaking;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ComboBox cmbStopbits;
        private System.Windows.Forms.ComboBox cmbParity;
        private System.Windows.Forms.ComboBox cmbDatabits;
        private System.Windows.Forms.ComboBox cmbBaud;
        private System.Windows.Forms.Label velocidadeLabel;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.ComboBox cmbPorta;
    }
}

