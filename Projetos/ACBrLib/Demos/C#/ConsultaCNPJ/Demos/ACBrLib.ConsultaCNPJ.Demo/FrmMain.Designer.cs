namespace ACBrLibConsultaCNPJ.Demo
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
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.rtbRespostas = new System.Windows.Forms.RichTextBox();
            this.tabControl3 = new System.Windows.Forms.TabControl();
            this.tabPage5 = new System.Windows.Forms.TabPage();
            this.label1 = new System.Windows.Forms.Label();
            this.edtCNPJ = new System.Windows.Forms.TextBox();
            this.cmbServico = new System.Windows.Forms.ComboBox();
            this.btnLerINI = new System.Windows.Forms.Button();
            this.btnSalvarINI = new System.Windows.Forms.Button();
            this.txtSenha = new System.Windows.Forms.TextBox();
            this.lblSenhaWebService = new System.Windows.Forms.Label();
            this.txtUsuario = new System.Windows.Forms.TextBox();
            this.lblUsuarioWebService = new System.Windows.Forms.Label();
            this.label43 = new System.Windows.Forms.Label();
            this.btnConsultarCNPJ = new System.Windows.Forms.Button();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.textPassword = new System.Windows.Forms.TextBox();
            this.textUser = new System.Windows.Forms.TextBox();
            this.textPort = new System.Windows.Forms.TextBox();
            this.textHost = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.groupBox2.SuspendLayout();
            this.tabControl3.SuspendLayout();
            this.tabPage5.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.rtbRespostas);
            this.groupBox2.Location = new System.Drawing.Point(321, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(400, 263);
            this.groupBox2.TabIndex = 20;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Respostas";
            // 
            // rtbRespostas
            // 
            this.rtbRespostas.Dock = System.Windows.Forms.DockStyle.Fill;
            this.rtbRespostas.Location = new System.Drawing.Point(3, 16);
            this.rtbRespostas.Name = "rtbRespostas";
            this.rtbRespostas.Size = new System.Drawing.Size(394, 244);
            this.rtbRespostas.TabIndex = 3;
            this.rtbRespostas.Text = "";
            // 
            // tabControl3
            // 
            this.tabControl3.Controls.Add(this.tabPage5);
            this.tabControl3.Controls.Add(this.tabPage1);
            this.tabControl3.Location = new System.Drawing.Point(12, 12);
            this.tabControl3.Name = "tabControl3";
            this.tabControl3.SelectedIndex = 0;
            this.tabControl3.Size = new System.Drawing.Size(307, 267);
            this.tabControl3.TabIndex = 25;
            // 
            // tabPage5
            // 
            this.tabPage5.Controls.Add(this.label1);
            this.tabPage5.Controls.Add(this.edtCNPJ);
            this.tabPage5.Controls.Add(this.cmbServico);
            this.tabPage5.Controls.Add(this.btnLerINI);
            this.tabPage5.Controls.Add(this.btnSalvarINI);
            this.tabPage5.Controls.Add(this.txtSenha);
            this.tabPage5.Controls.Add(this.lblSenhaWebService);
            this.tabPage5.Controls.Add(this.txtUsuario);
            this.tabPage5.Controls.Add(this.lblUsuarioWebService);
            this.tabPage5.Controls.Add(this.label43);
            this.tabPage5.Controls.Add(this.btnConsultarCNPJ);
            this.tabPage5.Location = new System.Drawing.Point(4, 22);
            this.tabPage5.Name = "tabPage5";
            this.tabPage5.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage5.Size = new System.Drawing.Size(299, 241);
            this.tabPage5.TabIndex = 0;
            this.tabPage5.Text = "Consultas";
            this.tabPage5.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 183);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(72, 13);
            this.label1.TabIndex = 56;
            this.label1.Text = "Informe CNPJ";
            // 
            // edtCNPJ
            // 
            this.edtCNPJ.Location = new System.Drawing.Point(6, 199);
            this.edtCNPJ.Name = "edtCNPJ";
            this.edtCNPJ.Size = new System.Drawing.Size(129, 20);
            this.edtCNPJ.TabIndex = 55;
            // 
            // cmbServico
            // 
            this.cmbServico.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbServico.FormattingEnabled = true;
            this.cmbServico.Items.AddRange(new object[] {
            "cwsNenhum",
            "cwsBrasilAPI",
            "cwsReceitaWS",
            "cwsCNPJWS"});
            this.cmbServico.Location = new System.Drawing.Point(15, 23);
            this.cmbServico.Name = "cmbServico";
            this.cmbServico.Size = new System.Drawing.Size(121, 21);
            this.cmbServico.TabIndex = 53;
            // 
            // btnLerINI
            // 
            this.btnLerINI.Location = new System.Drawing.Point(15, 93);
            this.btnLerINI.Name = "btnLerINI";
            this.btnLerINI.Size = new System.Drawing.Size(121, 25);
            this.btnLerINI.TabIndex = 52;
            this.btnLerINI.Text = "Ler Configurações";
            this.btnLerINI.UseVisualStyleBackColor = true;
            this.btnLerINI.Click += new System.EventHandler(this.btnLerINI_Click);
            // 
            // btnSalvarINI
            // 
            this.btnSalvarINI.Location = new System.Drawing.Point(159, 93);
            this.btnSalvarINI.Name = "btnSalvarINI";
            this.btnSalvarINI.Size = new System.Drawing.Size(129, 26);
            this.btnSalvarINI.TabIndex = 51;
            this.btnSalvarINI.Text = "Gravar Configurações";
            this.btnSalvarINI.UseVisualStyleBackColor = true;
            this.btnSalvarINI.Click += new System.EventHandler(this.btnSalvarINI_Click);
            // 
            // txtSenha
            // 
            this.txtSenha.Location = new System.Drawing.Point(160, 67);
            this.txtSenha.Name = "txtSenha";
            this.txtSenha.Size = new System.Drawing.Size(100, 20);
            this.txtSenha.TabIndex = 50;
            // 
            // lblSenhaWebService
            // 
            this.lblSenhaWebService.AutoSize = true;
            this.lblSenhaWebService.Location = new System.Drawing.Point(157, 51);
            this.lblSenhaWebService.Name = "lblSenhaWebService";
            this.lblSenhaWebService.Size = new System.Drawing.Size(38, 13);
            this.lblSenhaWebService.TabIndex = 49;
            this.lblSenhaWebService.Text = "Senha";
            // 
            // txtUsuario
            // 
            this.txtUsuario.Location = new System.Drawing.Point(160, 24);
            this.txtUsuario.Name = "txtUsuario";
            this.txtUsuario.Size = new System.Drawing.Size(100, 20);
            this.txtUsuario.TabIndex = 48;
            // 
            // lblUsuarioWebService
            // 
            this.lblUsuarioWebService.AutoSize = true;
            this.lblUsuarioWebService.Location = new System.Drawing.Point(157, 8);
            this.lblUsuarioWebService.Name = "lblUsuarioWebService";
            this.lblUsuarioWebService.Size = new System.Drawing.Size(43, 13);
            this.lblUsuarioWebService.TabIndex = 47;
            this.lblUsuarioWebService.Text = "Usuário";
            // 
            // label43
            // 
            this.label43.AutoSize = true;
            this.label43.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label43.Location = new System.Drawing.Point(12, 7);
            this.label43.Name = "label43";
            this.label43.Size = new System.Drawing.Size(50, 13);
            this.label43.TabIndex = 45;
            this.label43.Text = "Serviço";
            // 
            // btnConsultarCNPJ
            // 
            this.btnConsultarCNPJ.Location = new System.Drawing.Point(179, 196);
            this.btnConsultarCNPJ.Name = "btnConsultarCNPJ";
            this.btnConsultarCNPJ.Size = new System.Drawing.Size(109, 23);
            this.btnConsultarCNPJ.TabIndex = 0;
            this.btnConsultarCNPJ.Text = "Consultar CNPJ";
            this.btnConsultarCNPJ.UseVisualStyleBackColor = true;
            this.btnConsultarCNPJ.Click += new System.EventHandler(this.btnConsultarCNPJ_Click);
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.textPassword);
            this.tabPage1.Controls.Add(this.textUser);
            this.tabPage1.Controls.Add(this.textPort);
            this.tabPage1.Controls.Add(this.textHost);
            this.tabPage1.Controls.Add(this.label5);
            this.tabPage1.Controls.Add(this.label4);
            this.tabPage1.Controls.Add(this.label3);
            this.tabPage1.Controls.Add(this.label2);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(299, 241);
            this.tabPage1.TabIndex = 1;
            this.tabPage1.Text = "Proxy";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // textPassword
            // 
            this.textPassword.Location = new System.Drawing.Point(105, 148);
            this.textPassword.Name = "textPassword";
            this.textPassword.Size = new System.Drawing.Size(100, 20);
            this.textPassword.TabIndex = 7;
            // 
            // textUser
            // 
            this.textUser.Location = new System.Drawing.Point(105, 112);
            this.textUser.Name = "textUser";
            this.textUser.Size = new System.Drawing.Size(100, 20);
            this.textUser.TabIndex = 6;
            // 
            // textPort
            // 
            this.textPort.Location = new System.Drawing.Point(105, 71);
            this.textPort.Name = "textPort";
            this.textPort.Size = new System.Drawing.Size(50, 20);
            this.textPort.TabIndex = 5;
            // 
            // textHost
            // 
            this.textHost.Location = new System.Drawing.Point(105, 31);
            this.textHost.Name = "textHost";
            this.textHost.Size = new System.Drawing.Size(105, 20);
            this.textHost.TabIndex = 4;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(42, 151);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(38, 13);
            this.label5.TabIndex = 3;
            this.label5.Text = "Senha";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(42, 115);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(43, 13);
            this.label4.TabIndex = 2;
            this.label4.Text = "Usuário";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(42, 74);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(32, 13);
            this.label3.TabIndex = 1;
            this.label3.Text = "Porta";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(42, 34);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(46, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "Servidor";
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(733, 296);
            this.Controls.Add(this.tabControl3);
            this.Controls.Add(this.groupBox2);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibConsultaCNPJ Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.groupBox2.ResumeLayout(false);
            this.tabControl3.ResumeLayout(false);
            this.tabPage5.ResumeLayout(false);
            this.tabPage5.PerformLayout();
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RichTextBox rtbRespostas;
        private System.Windows.Forms.TabControl tabControl3;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.Button btnConsultarCNPJ;
        private System.Windows.Forms.Label label43;
        private System.Windows.Forms.Button btnLerINI;
        private System.Windows.Forms.Button btnSalvarINI;
        private System.Windows.Forms.TextBox txtSenha;
        private System.Windows.Forms.Label lblSenhaWebService;
        private System.Windows.Forms.TextBox txtUsuario;
        private System.Windows.Forms.Label lblUsuarioWebService;
        private System.Windows.Forms.ComboBox cmbServico;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox edtCNPJ;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TextBox textPassword;
        private System.Windows.Forms.TextBox textUser;
        private System.Windows.Forms.TextBox textPort;
        private System.Windows.Forms.TextBox textHost;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label2;
    }
}

