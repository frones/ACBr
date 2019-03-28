namespace ACBrLibNFe.Demo
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
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.rtbRespostas = new System.Windows.Forms.RichTextBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.label3 = new System.Windows.Forms.Label();
            this.nudTimeOut = new System.Windows.Forms.NumericUpDown();
            this.label2 = new System.Windows.Forms.Label();
            this.cmbSSlType = new System.Windows.Forms.ComboBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.rdbProducao = new System.Windows.Forms.RadioButton();
            this.rdbHomologacao = new System.Windows.Forms.RadioButton();
            this.cmbUfDestino = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.btnSalvar = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.cmbCrypt = new System.Windows.Forms.ComboBox();
            this.cmbHttp = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.cmbXmlSign = new System.Windows.Forms.ComboBox();
            this.label6 = new System.Windows.Forms.Label();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.txtCertPath = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.btnSelecionarCertificado = new System.Windows.Forms.Button();
            this.txtCertPassword = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.txtCertNumero = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.txtSchemaPath = new System.Windows.Forms.TextBox();
            this.btnSelectSchema = new System.Windows.Forms.Button();
            this.btnStatusServ = new System.Windows.Forms.Button();
            this.groupBox1.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudTimeOut)).BeginInit();
            this.groupBox3.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.btnStatusServ);
            this.groupBox1.Location = new System.Drawing.Point(242, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(289, 254);
            this.groupBox1.TabIndex = 1;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "NFe";
            // 
            // groupBox2
            // 
            this.groupBox2.Location = new System.Drawing.Point(537, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(269, 254);
            this.groupBox2.TabIndex = 2;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "NFCe";
            // 
            // rtbRespostas
            // 
            this.rtbRespostas.Location = new System.Drawing.Point(242, 272);
            this.rtbRespostas.Name = "rtbRespostas";
            this.rtbRespostas.Size = new System.Drawing.Size(564, 208);
            this.rtbRespostas.TabIndex = 3;
            this.rtbRespostas.Text = "";
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Location = new System.Drawing.Point(12, 12);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(224, 403);
            this.tabControl1.TabIndex = 4;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.label10);
            this.tabPage1.Controls.Add(this.txtSchemaPath);
            this.tabPage1.Controls.Add(this.btnSelectSchema);
            this.tabPage1.Controls.Add(this.groupBox4);
            this.tabPage1.Controls.Add(this.cmbXmlSign);
            this.tabPage1.Controls.Add(this.label6);
            this.tabPage1.Controls.Add(this.cmbHttp);
            this.tabPage1.Controls.Add(this.label5);
            this.tabPage1.Controls.Add(this.cmbCrypt);
            this.tabPage1.Controls.Add(this.label4);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(216, 377);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Configurações";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.label3);
            this.tabPage2.Controls.Add(this.nudTimeOut);
            this.tabPage2.Controls.Add(this.label2);
            this.tabPage2.Controls.Add(this.cmbSSlType);
            this.tabPage2.Controls.Add(this.groupBox3);
            this.tabPage2.Controls.Add(this.cmbUfDestino);
            this.tabPage2.Controls.Add(this.label1);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(216, 377);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Webservice";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(3, 137);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(54, 13);
            this.label3.TabIndex = 6;
            this.label3.Text = "TimeOut";
            // 
            // nudTimeOut
            // 
            this.nudTimeOut.Location = new System.Drawing.Point(6, 153);
            this.nudTimeOut.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.nudTimeOut.Minimum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.nudTimeOut.Name = "nudTimeOut";
            this.nudTimeOut.Size = new System.Drawing.Size(67, 20);
            this.nudTimeOut.TabIndex = 5;
            this.nudTimeOut.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.nudTimeOut.Value = new decimal(new int[] {
            5000,
            0,
            0,
            0});
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(3, 97);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(62, 13);
            this.label2.TabIndex = 4;
            this.label2.Text = "SSL Type";
            // 
            // cmbSSlType
            // 
            this.cmbSSlType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSSlType.FormattingEnabled = true;
            this.cmbSSlType.Items.AddRange(new object[] {
            "LT_all",
            "LT_SSLv2",
            "LT_SSLv3",
            "LT_TLSv1",
            "LT_TLSv1_1",
            "LT_TLSv1_2",
            "LT_SSHv2"});
            this.cmbSSlType.Location = new System.Drawing.Point(6, 113);
            this.cmbSSlType.Name = "cmbSSlType";
            this.cmbSSlType.Size = new System.Drawing.Size(204, 21);
            this.cmbSSlType.TabIndex = 3;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.rdbProducao);
            this.groupBox3.Controls.Add(this.rdbHomologacao);
            this.groupBox3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox3.Location = new System.Drawing.Point(6, 46);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(204, 48);
            this.groupBox3.TabIndex = 2;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Ambiente";
            // 
            // rdbProducao
            // 
            this.rdbProducao.AutoSize = true;
            this.rdbProducao.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbProducao.Location = new System.Drawing.Point(112, 19);
            this.rdbProducao.Name = "rdbProducao";
            this.rdbProducao.Size = new System.Drawing.Size(71, 17);
            this.rdbProducao.TabIndex = 1;
            this.rdbProducao.Text = "Produção";
            this.rdbProducao.UseVisualStyleBackColor = true;
            // 
            // rdbHomologacao
            // 
            this.rdbHomologacao.AutoSize = true;
            this.rdbHomologacao.Checked = true;
            this.rdbHomologacao.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbHomologacao.Location = new System.Drawing.Point(6, 19);
            this.rdbHomologacao.Name = "rdbHomologacao";
            this.rdbHomologacao.Size = new System.Drawing.Size(91, 17);
            this.rdbHomologacao.TabIndex = 0;
            this.rdbHomologacao.TabStop = true;
            this.rdbHomologacao.Text = "Homologação";
            this.rdbHomologacao.UseVisualStyleBackColor = true;
            // 
            // cmbUfDestino
            // 
            this.cmbUfDestino.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbUfDestino.FormattingEnabled = true;
            this.cmbUfDestino.Items.AddRange(new object[] {
            "AC",
            "AL",
            "AM",
            "AP",
            "BA",
            "CE",
            "DF",
            "ES",
            "GO",
            "MA",
            "MG",
            "MS",
            "MT",
            "PA",
            "PB",
            "PE",
            "PI",
            "PR",
            "RJ",
            "RN",
            "RO",
            "RR",
            "RS",
            "SC",
            "SE",
            "SP",
            "TO"});
            this.cmbUfDestino.Location = new System.Drawing.Point(6, 19);
            this.cmbUfDestino.Name = "cmbUfDestino";
            this.cmbUfDestino.Size = new System.Drawing.Size(204, 21);
            this.cmbUfDestino.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(6, 3);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(67, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Uf Destino";
            // 
            // btnSalvar
            // 
            this.btnSalvar.Location = new System.Drawing.Point(46, 417);
            this.btnSalvar.Name = "btnSalvar";
            this.btnSalvar.Size = new System.Drawing.Size(139, 23);
            this.btnSalvar.TabIndex = 5;
            this.btnSalvar.Text = "Salvar Configurações";
            this.btnSalvar.UseVisualStyleBackColor = true;
            this.btnSalvar.Click += new System.EventHandler(this.BtnSalvar_Click);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(6, 9);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(53, 13);
            this.label4.TabIndex = 1;
            this.label4.Text = "CryptLib";
            // 
            // cmbCrypt
            // 
            this.cmbCrypt.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbCrypt.FormattingEnabled = true;
            this.cmbCrypt.Items.AddRange(new object[] {
            "cryNone",
            "cryOpenSSL",
            "cryCapicom",
            "cryWinCrypt"});
            this.cmbCrypt.Location = new System.Drawing.Point(81, 6);
            this.cmbCrypt.Name = "cmbCrypt";
            this.cmbCrypt.Size = new System.Drawing.Size(129, 21);
            this.cmbCrypt.TabIndex = 2;
            // 
            // cmbHttp
            // 
            this.cmbHttp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbHttp.FormattingEnabled = true;
            this.cmbHttp.Items.AddRange(new object[] {
            "httpNone",
            "httpWinINet",
            "httpWinHttp",
            "httpOpenSSL",
            "httpIndy"});
            this.cmbHttp.Location = new System.Drawing.Point(81, 33);
            this.cmbHttp.Name = "cmbHttp";
            this.cmbHttp.Size = new System.Drawing.Size(129, 21);
            this.cmbHttp.TabIndex = 4;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(6, 36);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(48, 13);
            this.label5.TabIndex = 3;
            this.label5.Text = "HttpLib";
            // 
            // cmbXmlSign
            // 
            this.cmbXmlSign.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbXmlSign.FormattingEnabled = true;
            this.cmbXmlSign.Items.AddRange(new object[] {
            "xsNone",
            "xsXmlSec",
            "xsMsXml",
            "xsMsXmlCapicom",
            "xsLibXml2"});
            this.cmbXmlSign.Location = new System.Drawing.Point(81, 60);
            this.cmbXmlSign.Name = "cmbXmlSign";
            this.cmbXmlSign.Size = new System.Drawing.Size(129, 21);
            this.cmbXmlSign.TabIndex = 6;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(6, 63);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(69, 13);
            this.label6.TabIndex = 5;
            this.label6.Text = "XmlSignLib";
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.txtCertNumero);
            this.groupBox4.Controls.Add(this.label9);
            this.groupBox4.Controls.Add(this.label8);
            this.groupBox4.Controls.Add(this.txtCertPassword);
            this.groupBox4.Controls.Add(this.label7);
            this.groupBox4.Controls.Add(this.txtCertPath);
            this.groupBox4.Controls.Add(this.btnSelecionarCertificado);
            this.groupBox4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox4.Location = new System.Drawing.Point(6, 87);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(204, 145);
            this.groupBox4.TabIndex = 7;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Certificados";
            // 
            // txtCertPath
            // 
            this.txtCertPath.Location = new System.Drawing.Point(6, 32);
            this.txtCertPath.Name = "txtCertPath";
            this.txtCertPath.Size = new System.Drawing.Size(166, 20);
            this.txtCertPath.TabIndex = 6;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label7.Location = new System.Drawing.Point(3, 16);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(55, 13);
            this.label7.TabIndex = 8;
            this.label7.Text = "Caminho";
            // 
            // btnSelecionarCertificado
            // 
            this.btnSelecionarCertificado.Location = new System.Drawing.Point(171, 31);
            this.btnSelecionarCertificado.Name = "btnSelecionarCertificado";
            this.btnSelecionarCertificado.Size = new System.Drawing.Size(27, 22);
            this.btnSelecionarCertificado.TabIndex = 9;
            this.btnSelecionarCertificado.Text = "...";
            this.btnSelecionarCertificado.UseVisualStyleBackColor = true;
            this.btnSelecionarCertificado.Click += new System.EventHandler(this.BtnSelecionarCertificado_Click);
            // 
            // txtCertPassword
            // 
            this.txtCertPassword.Location = new System.Drawing.Point(6, 71);
            this.txtCertPassword.Name = "txtCertPassword";
            this.txtCertPassword.PasswordChar = '*';
            this.txtCertPassword.Size = new System.Drawing.Size(192, 20);
            this.txtCertPassword.TabIndex = 10;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label8.Location = new System.Drawing.Point(3, 55);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(43, 13);
            this.label8.TabIndex = 11;
            this.label8.Text = "Senha";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label9.Location = new System.Drawing.Point(3, 94);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(101, 13);
            this.label9.TabIndex = 12;
            this.label9.Text = "Número de Série";
            // 
            // txtCertNumero
            // 
            this.txtCertNumero.Location = new System.Drawing.Point(6, 110);
            this.txtCertNumero.Name = "txtCertNumero";
            this.txtCertNumero.Size = new System.Drawing.Size(192, 20);
            this.txtCertNumero.TabIndex = 13;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label10.Location = new System.Drawing.Point(6, 241);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(118, 13);
            this.label10.TabIndex = 11;
            this.label10.Text = "Pasta dos Schemas";
            // 
            // txtSchemaPath
            // 
            this.txtSchemaPath.Location = new System.Drawing.Point(6, 257);
            this.txtSchemaPath.Name = "txtSchemaPath";
            this.txtSchemaPath.Size = new System.Drawing.Size(178, 20);
            this.txtSchemaPath.TabIndex = 10;
            // 
            // btnSelectSchema
            // 
            this.btnSelectSchema.Location = new System.Drawing.Point(183, 256);
            this.btnSelectSchema.Name = "btnSelectSchema";
            this.btnSelectSchema.Size = new System.Drawing.Size(27, 22);
            this.btnSelectSchema.TabIndex = 12;
            this.btnSelectSchema.Text = "...";
            this.btnSelectSchema.UseVisualStyleBackColor = true;
            this.btnSelectSchema.Click += new System.EventHandler(this.BtnSelectSchema_Click);
            // 
            // btnStatusServ
            // 
            this.btnStatusServ.Location = new System.Drawing.Point(6, 19);
            this.btnStatusServ.Name = "btnStatusServ";
            this.btnStatusServ.Size = new System.Drawing.Size(118, 23);
            this.btnStatusServ.TabIndex = 0;
            this.btnStatusServ.Text = " Status de Serviço";
            this.btnStatusServ.UseVisualStyleBackColor = true;
            this.btnStatusServ.Click += new System.EventHandler(this.BtnStatusServ_Click);
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(818, 492);
            this.Controls.Add(this.btnSalvar);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.rtbRespostas);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibNFe Demo";
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.groupBox1.ResumeLayout(false);
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudTimeOut)).EndInit();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RichTextBox rtbRespostas;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.RadioButton rdbProducao;
        private System.Windows.Forms.RadioButton rdbHomologacao;
        private System.Windows.Forms.ComboBox cmbUfDestino;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox cmbSSlType;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.NumericUpDown nudTimeOut;
        private System.Windows.Forms.Button btnSalvar;
        private System.Windows.Forms.ComboBox cmbXmlSign;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ComboBox cmbHttp;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.ComboBox cmbCrypt;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.TextBox txtCertNumero;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox txtCertPassword;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox txtCertPath;
        private System.Windows.Forms.Button btnSelecionarCertificado;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox txtSchemaPath;
        private System.Windows.Forms.Button btnSelectSchema;
        private System.Windows.Forms.Button btnStatusServ;
    }
}

