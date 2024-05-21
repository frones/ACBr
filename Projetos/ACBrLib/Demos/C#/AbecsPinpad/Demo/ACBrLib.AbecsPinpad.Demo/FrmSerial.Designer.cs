namespace ACBrLibAbecsPinpad.Demo
{
    partial class FrmSerial
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FrmSerial));
            this.cmbVelocidade = new System.Windows.Forms.ComboBox();
            this.velocidadeLabel = new System.Windows.Forms.Label();
            this.timeOutNumericUpDown = new System.Windows.Forms.NumericUpDown();
            this.cmbPorta = new System.Windows.Forms.ComboBox();
            this.timeOutLabel = new System.Windows.Forms.Label();
            this.portaLabel = new System.Windows.Forms.Label();
            this.cmbDatabits = new System.Windows.Forms.ComboBox();
            this.cmbParity = new System.Windows.Forms.ComboBox();
            this.cmbStopbits = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.cmbHandshaking = new System.Windows.Forms.ComboBox();
            this.chkHardFlow = new System.Windows.Forms.CheckBox();
            this.chkSoftFlow = new System.Windows.Forms.CheckBox();
            this.btnConfirmar = new System.Windows.Forms.Button();
            this.btnCancelar = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.timeOutNumericUpDown)).BeginInit();
            this.SuspendLayout();
            // 
            // cmbVelocidade
            // 
            this.cmbVelocidade.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbVelocidade.FormattingEnabled = true;
            this.cmbVelocidade.Location = new System.Drawing.Point(144, 25);
            this.cmbVelocidade.Name = "cmbVelocidade";
            this.cmbVelocidade.Size = new System.Drawing.Size(109, 21);
            this.cmbVelocidade.TabIndex = 20;
            this.cmbVelocidade.SelectedIndexChanged += new System.EventHandler(this.cmbVelocidade_SelectedIndexChanged);
            // 
            // velocidadeLabel
            // 
            this.velocidadeLabel.AutoSize = true;
            this.velocidadeLabel.Location = new System.Drawing.Point(141, 9);
            this.velocidadeLabel.Name = "velocidadeLabel";
            this.velocidadeLabel.Size = new System.Drawing.Size(63, 13);
            this.velocidadeLabel.TabIndex = 19;
            this.velocidadeLabel.Text = "Velocidade:";
            // 
            // timeOutNumericUpDown
            // 
            this.timeOutNumericUpDown.Location = new System.Drawing.Point(304, 106);
            this.timeOutNumericUpDown.Name = "timeOutNumericUpDown";
            this.timeOutNumericUpDown.Size = new System.Drawing.Size(72, 20);
            this.timeOutNumericUpDown.TabIndex = 16;
            this.timeOutNumericUpDown.Value = new decimal(new int[] {
            3,
            0,
            0,
            0});
            this.timeOutNumericUpDown.ValueChanged += new System.EventHandler(this.timeOutNumericUpDown_ValueChanged);
            // 
            // cmbPorta
            // 
            this.cmbPorta.FormattingEnabled = true;
            this.cmbPorta.Location = new System.Drawing.Point(12, 25);
            this.cmbPorta.Name = "cmbPorta";
            this.cmbPorta.Size = new System.Drawing.Size(109, 21);
            this.cmbPorta.TabIndex = 14;
            this.cmbPorta.SelectedIndexChanged += new System.EventHandler(this.cmbPorta_SelectedIndexChanged);
            // 
            // timeOutLabel
            // 
            this.timeOutLabel.AutoSize = true;
            this.timeOutLabel.Location = new System.Drawing.Point(301, 90);
            this.timeOutLabel.Name = "timeOutLabel";
            this.timeOutLabel.Size = new System.Drawing.Size(50, 13);
            this.timeOutLabel.TabIndex = 15;
            this.timeOutLabel.Text = "TimeOut:";
            // 
            // portaLabel
            // 
            this.portaLabel.AutoSize = true;
            this.portaLabel.Location = new System.Drawing.Point(9, 9);
            this.portaLabel.Name = "portaLabel";
            this.portaLabel.Size = new System.Drawing.Size(35, 13);
            this.portaLabel.TabIndex = 13;
            this.portaLabel.Text = "Porta:";
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
            this.cmbDatabits.Location = new System.Drawing.Point(12, 65);
            this.cmbDatabits.Name = "cmbDatabits";
            this.cmbDatabits.Size = new System.Drawing.Size(109, 21);
            this.cmbDatabits.TabIndex = 21;
            this.cmbDatabits.SelectedIndexChanged += new System.EventHandler(this.cmbDatabits_SelectedIndexChanged);
            // 
            // cmbParity
            // 
            this.cmbParity.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbParity.FormattingEnabled = true;
            this.cmbParity.Location = new System.Drawing.Point(12, 105);
            this.cmbParity.Name = "cmbParity";
            this.cmbParity.Size = new System.Drawing.Size(109, 21);
            this.cmbParity.TabIndex = 22;
            this.cmbParity.SelectedIndexChanged += new System.EventHandler(this.cmbParity_SelectedIndexChanged);
            // 
            // cmbStopbits
            // 
            this.cmbStopbits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbStopbits.FormattingEnabled = true;
            this.cmbStopbits.Location = new System.Drawing.Point(144, 65);
            this.cmbStopbits.Name = "cmbStopbits";
            this.cmbStopbits.Size = new System.Drawing.Size(109, 21);
            this.cmbStopbits.TabIndex = 23;
            this.cmbStopbits.SelectedIndexChanged += new System.EventHandler(this.cmbStopbits_SelectedIndexChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(9, 49);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(125, 13);
            this.label1.TabIndex = 24;
            this.label1.Text = "&Data Bits (Bits de Dados)";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(9, 89);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(84, 13);
            this.label2.TabIndex = 25;
            this.label2.Text = "&Parity (Paridade)";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(141, 49);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(127, 13);
            this.label3.TabIndex = 26;
            this.label3.Text = "&Stop Bits (Bits de Parada)";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(141, 89);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(161, 13);
            this.label4.TabIndex = 28;
            this.label4.Text = "&Handshaking (Controle de Fluxo)";
            // 
            // cmbHandshaking
            // 
            this.cmbHandshaking.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbHandshaking.FormattingEnabled = true;
            this.cmbHandshaking.Location = new System.Drawing.Point(144, 105);
            this.cmbHandshaking.Name = "cmbHandshaking";
            this.cmbHandshaking.Size = new System.Drawing.Size(109, 21);
            this.cmbHandshaking.TabIndex = 27;
            this.cmbHandshaking.SelectedIndexChanged += new System.EventHandler(this.cmbHandshaking_SelectedIndexChanged);
            // 
            // chkHardFlow
            // 
            this.chkHardFlow.AutoSize = true;
            this.chkHardFlow.Location = new System.Drawing.Point(12, 132);
            this.chkHardFlow.Name = "chkHardFlow";
            this.chkHardFlow.Size = new System.Drawing.Size(71, 17);
            this.chkHardFlow.TabIndex = 29;
            this.chkHardFlow.Text = "HardFlow";
            this.chkHardFlow.UseVisualStyleBackColor = true;
            this.chkHardFlow.CheckedChanged += new System.EventHandler(this.checkBox1_CheckedChanged);
            // 
            // chkSoftFlow
            // 
            this.chkSoftFlow.AutoSize = true;
            this.chkSoftFlow.Location = new System.Drawing.Point(186, 132);
            this.chkSoftFlow.Name = "chkSoftFlow";
            this.chkSoftFlow.Size = new System.Drawing.Size(67, 17);
            this.chkSoftFlow.TabIndex = 30;
            this.chkSoftFlow.Text = "SoftFlow";
            this.chkSoftFlow.UseVisualStyleBackColor = true;
            this.chkSoftFlow.CheckedChanged += new System.EventHandler(this.checkBox2_CheckedChanged);
            // 
            // btnConfirmar
            // 
            this.btnConfirmar.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btnConfirmar.Image = ((System.Drawing.Image)(resources.GetObject("btnConfirmar.Image")));
            this.btnConfirmar.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnConfirmar.Location = new System.Drawing.Point(301, 13);
            this.btnConfirmar.Name = "btnConfirmar";
            this.btnConfirmar.Size = new System.Drawing.Size(75, 23);
            this.btnConfirmar.TabIndex = 31;
            this.btnConfirmar.Text = "OK";
            this.btnConfirmar.UseVisualStyleBackColor = true;
            this.btnConfirmar.Click += new System.EventHandler(this.btnConfirmar_Click);
            // 
            // btnCancelar
            // 
            this.btnCancelar.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancelar.Image = ((System.Drawing.Image)(resources.GetObject("btnCancelar.Image")));
            this.btnCancelar.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancelar.Location = new System.Drawing.Point(301, 42);
            this.btnCancelar.Name = "btnCancelar";
            this.btnCancelar.Size = new System.Drawing.Size(75, 23);
            this.btnCancelar.TabIndex = 32;
            this.btnCancelar.Text = "Cancelar";
            this.btnCancelar.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancelar.UseVisualStyleBackColor = true;
            this.btnCancelar.Click += new System.EventHandler(this.btnCancelar_Click);
            // 
            // FrmSerial
            // 
            this.AcceptButton = this.btnConfirmar;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.btnCancelar;
            this.ClientSize = new System.Drawing.Size(385, 159);
            this.Controls.Add(this.btnCancelar);
            this.Controls.Add(this.btnConfirmar);
            this.Controls.Add(this.chkSoftFlow);
            this.Controls.Add(this.chkHardFlow);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.cmbHandshaking);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.cmbStopbits);
            this.Controls.Add(this.cmbParity);
            this.Controls.Add(this.cmbDatabits);
            this.Controls.Add(this.cmbVelocidade);
            this.Controls.Add(this.velocidadeLabel);
            this.Controls.Add(this.timeOutNumericUpDown);
            this.Controls.Add(this.cmbPorta);
            this.Controls.Add(this.timeOutLabel);
            this.Controls.Add(this.portaLabel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FrmSerial";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Porta Serial";
            this.Load += new System.EventHandler(this.SerialCFGForm_Load);
            ((System.ComponentModel.ISupportInitialize)(this.timeOutNumericUpDown)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ComboBox cmbVelocidade;
        private System.Windows.Forms.Label velocidadeLabel;
        private System.Windows.Forms.NumericUpDown timeOutNumericUpDown;
        private System.Windows.Forms.ComboBox cmbPorta;
        private System.Windows.Forms.Label timeOutLabel;
        private System.Windows.Forms.Label portaLabel;
        private System.Windows.Forms.ComboBox cmbDatabits;
        private System.Windows.Forms.ComboBox cmbParity;
        private System.Windows.Forms.ComboBox cmbStopbits;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.ComboBox cmbHandshaking;
        private System.Windows.Forms.CheckBox chkHardFlow;
        private System.Windows.Forms.CheckBox chkSoftFlow;
        private System.Windows.Forms.Button btnConfirmar;
        private System.Windows.Forms.Button btnCancelar;
    }
}