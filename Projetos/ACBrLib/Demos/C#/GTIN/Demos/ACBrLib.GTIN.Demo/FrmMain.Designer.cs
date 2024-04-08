namespace ACBrLibGTIN.Demo
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
            this.btnConsultarGTIN = new System.Windows.Forms.Button();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tbpConfiguracoes = new System.Windows.Forms.TabPage();
            this.tabControl2 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.ckbSalvarArqeResp = new System.Windows.Forms.CheckBox();
            this.txtSchemaPath = new System.Windows.Forms.TextBox();
            this.label33 = new System.Windows.Forms.Label();
            this.btnSelectSchema = new System.Windows.Forms.Button();
            this.label30 = new System.Windows.Forms.Label();
            this.txtFormatoAlerta = new System.Windows.Forms.TextBox();
            this.ckbExibirErroSchema = new System.Windows.Forms.CheckBox();
            this.txtLogs = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.btnSelectLog = new System.Windows.Forms.Button();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.ckbSalvarSOAP = new System.Windows.Forms.CheckBox();
            this.ckbVisualizar = new System.Windows.Forms.CheckBox();
            this.groupBox5 = new System.Windows.Forms.GroupBox();
            this.txtProxySenha = new System.Windows.Forms.TextBox();
            this.label16 = new System.Windows.Forms.Label();
            this.txtProxyUsuario = new System.Windows.Forms.TextBox();
            this.label15 = new System.Windows.Forms.Label();
            this.label14 = new System.Windows.Forms.Label();
            this.nudProxyPorta = new System.Windows.Forms.NumericUpDown();
            this.txtProxyServidor = new System.Windows.Forms.TextBox();
            this.label13 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.nudTimeOut = new System.Windows.Forms.NumericUpDown();
            this.label2 = new System.Windows.Forms.Label();
            this.cmbSSlType = new System.Windows.Forms.ComboBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.rdbProducao = new System.Windows.Forms.RadioButton();
            this.rdbHomologacao = new System.Windows.Forms.RadioButton();
            this.cmbUfDestino = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.label26 = new System.Windows.Forms.Label();
            this.txtDadosPFX = new System.Windows.Forms.TextBox();
            this.btnDadosPFX = new System.Windows.Forms.Button();
            this.txtCertNumero = new System.Windows.Forms.TextBox();
            this.label9 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.txtCertPassword = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.txtCertPath = new System.Windows.Forms.TextBox();
            this.btnSelecionarCertificado = new System.Windows.Forms.Button();
            this.cmbXmlSign = new System.Windows.Forms.ComboBox();
            this.label6 = new System.Windows.Forms.Label();
            this.cmbHttp = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.cmbCrypt = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.tabPage4 = new System.Windows.Forms.TabPage();
            this.txtArqGTIN = new System.Windows.Forms.TextBox();
            this.btnArqGTIN = new System.Windows.Forms.Button();
            this.label36 = new System.Windows.Forms.Label();
            this.ckbAdicionaLiteral = new System.Windows.Forms.CheckBox();
            this.ckbSalvarArqs = new System.Windows.Forms.CheckBox();
            this.btnCarregarConfiguracoes = new System.Windows.Forms.Button();
            this.btnSalvarConfiguracoes = new System.Windows.Forms.Button();
            this.btnOpenSSLInfo = new System.Windows.Forms.Button();
            this.groupBox2.SuspendLayout();
            this.tabControl3.SuspendLayout();
            this.tabPage5.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tbpConfiguracoes.SuspendLayout();
            this.tabControl2.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.groupBox5.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudProxyPorta)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudTimeOut)).BeginInit();
            this.groupBox3.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.tabPage4.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.rtbRespostas);
            this.groupBox2.Location = new System.Drawing.Point(333, 285);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(384, 211);
            this.groupBox2.TabIndex = 20;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Respostas";
            // 
            // rtbRespostas
            // 
            this.rtbRespostas.Dock = System.Windows.Forms.DockStyle.Fill;
            this.rtbRespostas.Location = new System.Drawing.Point(3, 16);
            this.rtbRespostas.Name = "rtbRespostas";
            this.rtbRespostas.Size = new System.Drawing.Size(378, 192);
            this.rtbRespostas.TabIndex = 3;
            this.rtbRespostas.Text = "";
            // 
            // tabControl3
            // 
            this.tabControl3.Controls.Add(this.tabPage5);
            this.tabControl3.Location = new System.Drawing.Point(329, 12);
            this.tabControl3.Name = "tabControl3";
            this.tabControl3.SelectedIndex = 0;
            this.tabControl3.Size = new System.Drawing.Size(392, 267);
            this.tabControl3.TabIndex = 25;
            // 
            // tabPage5
            // 
            this.tabPage5.Controls.Add(this.btnConsultarGTIN);
            this.tabPage5.Location = new System.Drawing.Point(4, 22);
            this.tabPage5.Name = "tabPage5";
            this.tabPage5.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage5.Size = new System.Drawing.Size(384, 241);
            this.tabPage5.TabIndex = 0;
            this.tabPage5.Text = "Consultas";
            this.tabPage5.UseVisualStyleBackColor = true;
            // 
            // btnConsultarGTIN
            // 
            this.btnConsultarGTIN.Location = new System.Drawing.Point(21, 15);
            this.btnConsultarGTIN.Name = "btnConsultarGTIN";
            this.btnConsultarGTIN.Size = new System.Drawing.Size(109, 23);
            this.btnConsultarGTIN.TabIndex = 0;
            this.btnConsultarGTIN.Text = "Consultar GTIN";
            this.btnConsultarGTIN.UseVisualStyleBackColor = true;
            this.btnConsultarGTIN.Click += new System.EventHandler(this.btnConsultarGTIN_Click);
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tbpConfiguracoes);
            this.tabControl1.Location = new System.Drawing.Point(12, 12);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(311, 484);
            this.tabControl1.TabIndex = 26;
            // 
            // tbpConfiguracoes
            // 
            this.tbpConfiguracoes.Controls.Add(this.tabControl2);
            this.tbpConfiguracoes.Location = new System.Drawing.Point(4, 22);
            this.tbpConfiguracoes.Name = "tbpConfiguracoes";
            this.tbpConfiguracoes.Padding = new System.Windows.Forms.Padding(3);
            this.tbpConfiguracoes.Size = new System.Drawing.Size(303, 458);
            this.tbpConfiguracoes.TabIndex = 2;
            this.tbpConfiguracoes.Text = "Configurações";
            this.tbpConfiguracoes.UseVisualStyleBackColor = true;
            // 
            // tabControl2
            // 
            this.tabControl2.Controls.Add(this.tabPage1);
            this.tabControl2.Controls.Add(this.tabPage2);
            this.tabControl2.Controls.Add(this.tabPage3);
            this.tabControl2.Controls.Add(this.tabPage4);
            this.tabControl2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl2.Location = new System.Drawing.Point(3, 3);
            this.tabControl2.Name = "tabControl2";
            this.tabControl2.SelectedIndex = 0;
            this.tabControl2.Size = new System.Drawing.Size(297, 452);
            this.tabControl2.TabIndex = 24;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.ckbSalvarArqeResp);
            this.tabPage1.Controls.Add(this.txtSchemaPath);
            this.tabPage1.Controls.Add(this.label33);
            this.tabPage1.Controls.Add(this.btnSelectSchema);
            this.tabPage1.Controls.Add(this.label30);
            this.tabPage1.Controls.Add(this.txtFormatoAlerta);
            this.tabPage1.Controls.Add(this.ckbExibirErroSchema);
            this.tabPage1.Controls.Add(this.txtLogs);
            this.tabPage1.Controls.Add(this.label10);
            this.tabPage1.Controls.Add(this.btnSelectLog);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(289, 426);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Geral";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // ckbSalvarArqeResp
            // 
            this.ckbSalvarArqeResp.AutoSize = true;
            this.ckbSalvarArqeResp.Location = new System.Drawing.Point(6, 85);
            this.ckbSalvarArqeResp.Name = "ckbSalvarArqeResp";
            this.ckbSalvarArqeResp.Size = new System.Drawing.Size(202, 17);
            this.ckbSalvarArqeResp.TabIndex = 36;
            this.ckbSalvarArqeResp.Text = "Salvar Arquivos de Envio e Resposta";
            this.ckbSalvarArqeResp.UseVisualStyleBackColor = true;
            // 
            // txtSchemaPath
            // 
            this.txtSchemaPath.Location = new System.Drawing.Point(6, 160);
            this.txtSchemaPath.Name = "txtSchemaPath";
            this.txtSchemaPath.Size = new System.Drawing.Size(238, 20);
            this.txtSchemaPath.TabIndex = 32;
            // 
            // label33
            // 
            this.label33.AutoSize = true;
            this.label33.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label33.Location = new System.Drawing.Point(3, 144);
            this.label33.Name = "label33";
            this.label33.Size = new System.Drawing.Size(118, 13);
            this.label33.TabIndex = 33;
            this.label33.Text = "Pasta dos Schemas";
            // 
            // btnSelectSchema
            // 
            this.btnSelectSchema.Location = new System.Drawing.Point(243, 159);
            this.btnSelectSchema.Name = "btnSelectSchema";
            this.btnSelectSchema.Size = new System.Drawing.Size(27, 22);
            this.btnSelectSchema.TabIndex = 34;
            this.btnSelectSchema.Text = "...";
            this.btnSelectSchema.UseVisualStyleBackColor = true;
            this.btnSelectSchema.Click += new System.EventHandler(this.btnSelectSchema_Click);
            // 
            // label30
            // 
            this.label30.AutoSize = true;
            this.label30.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label30.Location = new System.Drawing.Point(3, 43);
            this.label30.Name = "label30";
            this.label30.Size = new System.Drawing.Size(89, 13);
            this.label30.TabIndex = 26;
            this.label30.Text = "Formato Alerta";
            // 
            // txtFormatoAlerta
            // 
            this.txtFormatoAlerta.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFormatoAlerta.Location = new System.Drawing.Point(6, 59);
            this.txtFormatoAlerta.Name = "txtFormatoAlerta";
            this.txtFormatoAlerta.Size = new System.Drawing.Size(267, 20);
            this.txtFormatoAlerta.TabIndex = 27;
            // 
            // ckbExibirErroSchema
            // 
            this.ckbExibirErroSchema.AutoSize = true;
            this.ckbExibirErroSchema.Location = new System.Drawing.Point(6, 23);
            this.ckbExibirErroSchema.Name = "ckbExibirErroSchema";
            this.ckbExibirErroSchema.Size = new System.Drawing.Size(115, 17);
            this.ckbExibirErroSchema.TabIndex = 25;
            this.ckbExibirErroSchema.Text = "Exibir Erro Schema";
            this.ckbExibirErroSchema.UseVisualStyleBackColor = true;
            // 
            // txtLogs
            // 
            this.txtLogs.Location = new System.Drawing.Point(6, 121);
            this.txtLogs.Name = "txtLogs";
            this.txtLogs.Size = new System.Drawing.Size(238, 20);
            this.txtLogs.TabIndex = 13;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label10.Location = new System.Drawing.Point(3, 105);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(94, 13);
            this.label10.TabIndex = 14;
            this.label10.Text = "Pasta dos Logs";
            // 
            // btnSelectLog
            // 
            this.btnSelectLog.Location = new System.Drawing.Point(243, 120);
            this.btnSelectLog.Name = "btnSelectLog";
            this.btnSelectLog.Size = new System.Drawing.Size(27, 22);
            this.btnSelectLog.TabIndex = 15;
            this.btnSelectLog.Text = "...";
            this.btnSelectLog.UseVisualStyleBackColor = true;
            this.btnSelectLog.Click += new System.EventHandler(this.btnSelectLog_Click);
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.ckbSalvarSOAP);
            this.tabPage2.Controls.Add(this.ckbVisualizar);
            this.tabPage2.Controls.Add(this.groupBox5);
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
            this.tabPage2.Size = new System.Drawing.Size(289, 426);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Webservices";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // ckbSalvarSOAP
            // 
            this.ckbSalvarSOAP.AutoSize = true;
            this.ckbSalvarSOAP.Location = new System.Drawing.Point(6, 123);
            this.ckbSalvarSOAP.Name = "ckbSalvarSOAP";
            this.ckbSalvarSOAP.Size = new System.Drawing.Size(135, 17);
            this.ckbSalvarSOAP.TabIndex = 17;
            this.ckbSalvarSOAP.Text = "Salvar envelope SOAP";
            this.ckbSalvarSOAP.UseVisualStyleBackColor = true;
            // 
            // ckbVisualizar
            // 
            this.ckbVisualizar.AutoSize = true;
            this.ckbVisualizar.Location = new System.Drawing.Point(6, 100);
            this.ckbVisualizar.Name = "ckbVisualizar";
            this.ckbVisualizar.Size = new System.Drawing.Size(125, 17);
            this.ckbVisualizar.TabIndex = 16;
            this.ckbVisualizar.Text = "Visualizar Mensagem";
            this.ckbVisualizar.UseVisualStyleBackColor = true;
            // 
            // groupBox5
            // 
            this.groupBox5.Controls.Add(this.txtProxySenha);
            this.groupBox5.Controls.Add(this.label16);
            this.groupBox5.Controls.Add(this.txtProxyUsuario);
            this.groupBox5.Controls.Add(this.label15);
            this.groupBox5.Controls.Add(this.label14);
            this.groupBox5.Controls.Add(this.nudProxyPorta);
            this.groupBox5.Controls.Add(this.txtProxyServidor);
            this.groupBox5.Controls.Add(this.label13);
            this.groupBox5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox5.Location = new System.Drawing.Point(6, 146);
            this.groupBox5.Name = "groupBox5";
            this.groupBox5.Size = new System.Drawing.Size(267, 141);
            this.groupBox5.TabIndex = 13;
            this.groupBox5.TabStop = false;
            this.groupBox5.Text = "Proxy";
            // 
            // txtProxySenha
            // 
            this.txtProxySenha.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtProxySenha.Location = new System.Drawing.Point(9, 110);
            this.txtProxySenha.Name = "txtProxySenha";
            this.txtProxySenha.PasswordChar = '*';
            this.txtProxySenha.Size = new System.Drawing.Size(252, 20);
            this.txtProxySenha.TabIndex = 23;
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label16.Location = new System.Drawing.Point(6, 94);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(43, 13);
            this.label16.TabIndex = 22;
            this.label16.Text = "Senha";
            // 
            // txtProxyUsuario
            // 
            this.txtProxyUsuario.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtProxyUsuario.Location = new System.Drawing.Point(9, 71);
            this.txtProxyUsuario.Name = "txtProxyUsuario";
            this.txtProxyUsuario.Size = new System.Drawing.Size(252, 20);
            this.txtProxyUsuario.TabIndex = 21;
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label15.Location = new System.Drawing.Point(6, 55);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(50, 13);
            this.label15.TabIndex = 20;
            this.label15.Text = "Usuário";
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label14.Location = new System.Drawing.Point(199, 16);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(37, 13);
            this.label14.TabIndex = 19;
            this.label14.Text = "Porta";
            // 
            // nudProxyPorta
            // 
            this.nudProxyPorta.Location = new System.Drawing.Point(202, 33);
            this.nudProxyPorta.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.nudProxyPorta.Minimum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.nudProxyPorta.Name = "nudProxyPorta";
            this.nudProxyPorta.Size = new System.Drawing.Size(59, 20);
            this.nudProxyPorta.TabIndex = 18;
            this.nudProxyPorta.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.nudProxyPorta.Value = new decimal(new int[] {
            5000,
            0,
            0,
            0});
            // 
            // txtProxyServidor
            // 
            this.txtProxyServidor.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtProxyServidor.Location = new System.Drawing.Point(9, 32);
            this.txtProxyServidor.Name = "txtProxyServidor";
            this.txtProxyServidor.Size = new System.Drawing.Size(187, 20);
            this.txtProxyServidor.TabIndex = 17;
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label13.Location = new System.Drawing.Point(6, 16);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(54, 13);
            this.label13.TabIndex = 16;
            this.label13.Text = "Servidor";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(179, 3);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(54, 13);
            this.label3.TabIndex = 14;
            this.label3.Text = "TimeOut";
            // 
            // nudTimeOut
            // 
            this.nudTimeOut.Location = new System.Drawing.Point(182, 19);
            this.nudTimeOut.Maximum = new decimal(new int[] {
            999999999,
            0,
            0,
            0});
            this.nudTimeOut.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.nudTimeOut.Name = "nudTimeOut";
            this.nudTimeOut.Size = new System.Drawing.Size(73, 20);
            this.nudTimeOut.TabIndex = 12;
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
            this.label2.Location = new System.Drawing.Point(76, 3);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(62, 13);
            this.label2.TabIndex = 11;
            this.label2.Text = "SSL Type";
            // 
            // cmbSSlType
            // 
            this.cmbSSlType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSSlType.FormattingEnabled = true;
            this.cmbSSlType.Location = new System.Drawing.Point(79, 19);
            this.cmbSSlType.Name = "cmbSSlType";
            this.cmbSSlType.Size = new System.Drawing.Size(97, 21);
            this.cmbSSlType.TabIndex = 10;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.rdbProducao);
            this.groupBox3.Controls.Add(this.rdbHomologacao);
            this.groupBox3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox3.Location = new System.Drawing.Point(6, 46);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(267, 48);
            this.groupBox3.TabIndex = 9;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Ambiente";
            // 
            // rdbProducao
            // 
            this.rdbProducao.AutoSize = true;
            this.rdbProducao.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbProducao.Location = new System.Drawing.Point(6, 19);
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
            this.rdbHomologacao.Location = new System.Drawing.Point(170, 19);
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
            this.cmbUfDestino.Size = new System.Drawing.Size(67, 21);
            this.cmbUfDestino.TabIndex = 8;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(6, 3);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(67, 13);
            this.label1.TabIndex = 7;
            this.label1.Text = "Uf Destino";
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.btnOpenSSLInfo);
            this.tabPage3.Controls.Add(this.groupBox4);
            this.tabPage3.Controls.Add(this.cmbXmlSign);
            this.tabPage3.Controls.Add(this.label6);
            this.tabPage3.Controls.Add(this.cmbHttp);
            this.tabPage3.Controls.Add(this.label5);
            this.tabPage3.Controls.Add(this.cmbCrypt);
            this.tabPage3.Controls.Add(this.label4);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Size = new System.Drawing.Size(289, 426);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "Certificados";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.label26);
            this.groupBox4.Controls.Add(this.txtDadosPFX);
            this.groupBox4.Controls.Add(this.btnDadosPFX);
            this.groupBox4.Controls.Add(this.txtCertNumero);
            this.groupBox4.Controls.Add(this.label9);
            this.groupBox4.Controls.Add(this.label8);
            this.groupBox4.Controls.Add(this.txtCertPassword);
            this.groupBox4.Controls.Add(this.label7);
            this.groupBox4.Controls.Add(this.txtCertPath);
            this.groupBox4.Controls.Add(this.btnSelecionarCertificado);
            this.groupBox4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox4.Location = new System.Drawing.Point(6, 129);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(265, 180);
            this.groupBox4.TabIndex = 14;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Certificados";
            // 
            // label26
            // 
            this.label26.AutoSize = true;
            this.label26.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label26.Location = new System.Drawing.Point(3, 55);
            this.label26.Name = "label26";
            this.label26.Size = new System.Drawing.Size(70, 13);
            this.label26.TabIndex = 15;
            this.label26.Text = "Dados PFX";
            // 
            // txtDadosPFX
            // 
            this.txtDadosPFX.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtDadosPFX.Location = new System.Drawing.Point(6, 71);
            this.txtDadosPFX.MaxLength = 99999;
            this.txtDadosPFX.Name = "txtDadosPFX";
            this.txtDadosPFX.Size = new System.Drawing.Size(227, 20);
            this.txtDadosPFX.TabIndex = 14;
            // 
            // btnDadosPFX
            // 
            this.btnDadosPFX.Location = new System.Drawing.Point(232, 70);
            this.btnDadosPFX.Name = "btnDadosPFX";
            this.btnDadosPFX.Size = new System.Drawing.Size(27, 22);
            this.btnDadosPFX.TabIndex = 16;
            this.btnDadosPFX.Text = "...";
            this.btnDadosPFX.UseVisualStyleBackColor = true;
            this.btnDadosPFX.Click += new System.EventHandler(this.btnDadosPFX_Click);
            // 
            // txtCertNumero
            // 
            this.txtCertNumero.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCertNumero.Location = new System.Drawing.Point(6, 149);
            this.txtCertNumero.Name = "txtCertNumero";
            this.txtCertNumero.Size = new System.Drawing.Size(253, 20);
            this.txtCertNumero.TabIndex = 13;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label9.Location = new System.Drawing.Point(3, 133);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(101, 13);
            this.label9.TabIndex = 12;
            this.label9.Text = "Número de Série";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label8.Location = new System.Drawing.Point(3, 94);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(43, 13);
            this.label8.TabIndex = 11;
            this.label8.Text = "Senha";
            // 
            // txtCertPassword
            // 
            this.txtCertPassword.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCertPassword.Location = new System.Drawing.Point(6, 110);
            this.txtCertPassword.Name = "txtCertPassword";
            this.txtCertPassword.PasswordChar = '*';
            this.txtCertPassword.Size = new System.Drawing.Size(253, 20);
            this.txtCertPassword.TabIndex = 10;
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
            // txtCertPath
            // 
            this.txtCertPath.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCertPath.Location = new System.Drawing.Point(6, 32);
            this.txtCertPath.Name = "txtCertPath";
            this.txtCertPath.Size = new System.Drawing.Size(227, 20);
            this.txtCertPath.TabIndex = 6;
            // 
            // btnSelecionarCertificado
            // 
            this.btnSelecionarCertificado.Location = new System.Drawing.Point(232, 31);
            this.btnSelecionarCertificado.Name = "btnSelecionarCertificado";
            this.btnSelecionarCertificado.Size = new System.Drawing.Size(27, 22);
            this.btnSelecionarCertificado.TabIndex = 9;
            this.btnSelecionarCertificado.Text = "...";
            this.btnSelecionarCertificado.UseVisualStyleBackColor = true;
            this.btnSelecionarCertificado.Click += new System.EventHandler(this.btnSelecionarCertificado_Click);
            // 
            // cmbXmlSign
            // 
            this.cmbXmlSign.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbXmlSign.FormattingEnabled = true;
            this.cmbXmlSign.Location = new System.Drawing.Point(9, 102);
            this.cmbXmlSign.Name = "cmbXmlSign";
            this.cmbXmlSign.Size = new System.Drawing.Size(129, 21);
            this.cmbXmlSign.TabIndex = 13;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(6, 86);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(69, 13);
            this.label6.TabIndex = 12;
            this.label6.Text = "XmlSignLib";
            // 
            // cmbHttp
            // 
            this.cmbHttp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbHttp.FormattingEnabled = true;
            this.cmbHttp.Location = new System.Drawing.Point(9, 62);
            this.cmbHttp.Name = "cmbHttp";
            this.cmbHttp.Size = new System.Drawing.Size(129, 21);
            this.cmbHttp.TabIndex = 11;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(6, 46);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(48, 13);
            this.label5.TabIndex = 10;
            this.label5.Text = "HttpLib";
            // 
            // cmbCrypt
            // 
            this.cmbCrypt.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbCrypt.FormattingEnabled = true;
            this.cmbCrypt.Location = new System.Drawing.Point(9, 22);
            this.cmbCrypt.Name = "cmbCrypt";
            this.cmbCrypt.Size = new System.Drawing.Size(129, 21);
            this.cmbCrypt.TabIndex = 9;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(6, 6);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(53, 13);
            this.label4.TabIndex = 8;
            this.label4.Text = "CryptLib";
            // 
            // tabPage4
            // 
            this.tabPage4.Controls.Add(this.txtArqGTIN);
            this.tabPage4.Controls.Add(this.btnArqGTIN);
            this.tabPage4.Controls.Add(this.label36);
            this.tabPage4.Controls.Add(this.ckbAdicionaLiteral);
            this.tabPage4.Controls.Add(this.ckbSalvarArqs);
            this.tabPage4.Location = new System.Drawing.Point(4, 22);
            this.tabPage4.Name = "tabPage4";
            this.tabPage4.Size = new System.Drawing.Size(289, 426);
            this.tabPage4.TabIndex = 3;
            this.tabPage4.Text = "Arquivos";
            this.tabPage4.UseVisualStyleBackColor = true;
            // 
            // txtArqGTIN
            // 
            this.txtArqGTIN.Location = new System.Drawing.Point(3, 79);
            this.txtArqGTIN.Name = "txtArqGTIN";
            this.txtArqGTIN.Size = new System.Drawing.Size(248, 20);
            this.txtArqGTIN.TabIndex = 25;
            // 
            // btnArqGTIN
            // 
            this.btnArqGTIN.Location = new System.Drawing.Point(257, 77);
            this.btnArqGTIN.Name = "btnArqGTIN";
            this.btnArqGTIN.Size = new System.Drawing.Size(27, 22);
            this.btnArqGTIN.TabIndex = 27;
            this.btnArqGTIN.Text = "...";
            this.btnArqGTIN.UseVisualStyleBackColor = true;
            this.btnArqGTIN.Click += new System.EventHandler(this.btnArqGTIN_Click);
            // 
            // label36
            // 
            this.label36.AutoSize = true;
            this.label36.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label36.Location = new System.Drawing.Point(3, 62);
            this.label36.Name = "label36";
            this.label36.Size = new System.Drawing.Size(126, 13);
            this.label36.TabIndex = 26;
            this.label36.Text = "Pasta Arquivos GTIN";
            // 
            // ckbAdicionaLiteral
            // 
            this.ckbAdicionaLiteral.AutoSize = true;
            this.ckbAdicionaLiteral.Location = new System.Drawing.Point(6, 29);
            this.ckbAdicionaLiteral.Name = "ckbAdicionaLiteral";
            this.ckbAdicionaLiteral.Size = new System.Drawing.Size(199, 17);
            this.ckbAdicionaLiteral.TabIndex = 9;
            this.ckbAdicionaLiteral.Text = "Adicionar Literal no nome das pastas";
            this.ckbAdicionaLiteral.UseVisualStyleBackColor = true;
            // 
            // ckbSalvarArqs
            // 
            this.ckbSalvarArqs.AutoSize = true;
            this.ckbSalvarArqs.Location = new System.Drawing.Point(6, 6);
            this.ckbSalvarArqs.Name = "ckbSalvarArqs";
            this.ckbSalvarArqs.Size = new System.Drawing.Size(206, 17);
            this.ckbSalvarArqs.TabIndex = 7;
            this.ckbSalvarArqs.Text = "Salvar Arquivos em Pastas Separadas";
            this.ckbSalvarArqs.UseVisualStyleBackColor = true;
            // 
            // btnCarregarConfiguracoes
            // 
            this.btnCarregarConfiguracoes.Location = new System.Drawing.Point(16, 499);
            this.btnCarregarConfiguracoes.Name = "btnCarregarConfiguracoes";
            this.btnCarregarConfiguracoes.Size = new System.Drawing.Size(139, 23);
            this.btnCarregarConfiguracoes.TabIndex = 28;
            this.btnCarregarConfiguracoes.Text = "Carregar Configurações";
            this.btnCarregarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnCarregarConfiguracoes.Click += new System.EventHandler(this.btnCarregarConfiguracoes_Click);
            // 
            // btnSalvarConfiguracoes
            // 
            this.btnSalvarConfiguracoes.Location = new System.Drawing.Point(180, 498);
            this.btnSalvarConfiguracoes.Name = "btnSalvarConfiguracoes";
            this.btnSalvarConfiguracoes.Size = new System.Drawing.Size(139, 23);
            this.btnSalvarConfiguracoes.TabIndex = 27;
            this.btnSalvarConfiguracoes.Text = "Salvar Configurações";
            this.btnSalvarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnSalvarConfiguracoes.Click += new System.EventHandler(this.btnSalvarConfiguracoes_Click);
            // 
            // btnOpenSSLInfo
            // 
            this.btnOpenSSLInfo.Location = new System.Drawing.Point(6, 344);
            this.btnOpenSSLInfo.Name = "btnOpenSSLInfo";
            this.btnOpenSSLInfo.Size = new System.Drawing.Size(118, 23);
            this.btnOpenSSLInfo.TabIndex = 22;
            this.btnOpenSSLInfo.Text = "OpenSSLInfo";
            this.btnOpenSSLInfo.UseVisualStyleBackColor = true;
            this.btnOpenSSLInfo.Click += new System.EventHandler(this.btnOpenSSLInfo_Click);
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(733, 528);
            this.Controls.Add(this.btnCarregarConfiguracoes);
            this.Controls.Add(this.btnSalvarConfiguracoes);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.tabControl3);
            this.Controls.Add(this.groupBox2);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibGTIN Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.groupBox2.ResumeLayout(false);
            this.tabControl3.ResumeLayout(false);
            this.tabPage5.ResumeLayout(false);
            this.tabControl1.ResumeLayout(false);
            this.tbpConfiguracoes.ResumeLayout(false);
            this.tabControl2.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            this.groupBox5.ResumeLayout(false);
            this.groupBox5.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudProxyPorta)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudTimeOut)).EndInit();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.tabPage3.ResumeLayout(false);
            this.tabPage3.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.tabPage4.ResumeLayout(false);
            this.tabPage4.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RichTextBox rtbRespostas;
        private System.Windows.Forms.TabControl tabControl3;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tbpConfiguracoes;
        private System.Windows.Forms.TabControl tabControl2;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.CheckBox ckbSalvarArqeResp;
        private System.Windows.Forms.TextBox txtSchemaPath;
        private System.Windows.Forms.Label label33;
        private System.Windows.Forms.Button btnSelectSchema;
        private System.Windows.Forms.Label label30;
        private System.Windows.Forms.TextBox txtFormatoAlerta;
        private System.Windows.Forms.CheckBox ckbExibirErroSchema;
        private System.Windows.Forms.TextBox txtLogs;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Button btnSelectLog;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.CheckBox ckbSalvarSOAP;
        private System.Windows.Forms.CheckBox ckbVisualizar;
        private System.Windows.Forms.GroupBox groupBox5;
        private System.Windows.Forms.TextBox txtProxySenha;
        private System.Windows.Forms.Label label16;
        private System.Windows.Forms.TextBox txtProxyUsuario;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.NumericUpDown nudProxyPorta;
        private System.Windows.Forms.TextBox txtProxyServidor;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.NumericUpDown nudTimeOut;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox cmbSSlType;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.RadioButton rdbProducao;
        private System.Windows.Forms.RadioButton rdbHomologacao;
        private System.Windows.Forms.ComboBox cmbUfDestino;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.Label label26;
        private System.Windows.Forms.TextBox txtDadosPFX;
        private System.Windows.Forms.Button btnDadosPFX;
        private System.Windows.Forms.TextBox txtCertNumero;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox txtCertPassword;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox txtCertPath;
        private System.Windows.Forms.Button btnSelecionarCertificado;
        private System.Windows.Forms.ComboBox cmbXmlSign;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ComboBox cmbHttp;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.ComboBox cmbCrypt;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TabPage tabPage4;
        private System.Windows.Forms.TextBox txtArqGTIN;
        private System.Windows.Forms.Button btnArqGTIN;
        private System.Windows.Forms.Label label36;
        private System.Windows.Forms.CheckBox ckbAdicionaLiteral;
        private System.Windows.Forms.CheckBox ckbSalvarArqs;
        private System.Windows.Forms.Button btnCarregarConfiguracoes;
        private System.Windows.Forms.Button btnSalvarConfiguracoes;
        private System.Windows.Forms.Button btnConsultarGTIN;
        private System.Windows.Forms.Button btnOpenSSLInfo;
    }
}

