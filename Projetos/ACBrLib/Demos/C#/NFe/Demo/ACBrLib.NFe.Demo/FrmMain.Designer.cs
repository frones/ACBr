namespace ACBrLib.NFe.Demo
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
            this.rtbRespostas = new System.Windows.Forms.RichTextBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tbpConfiguracoes = new System.Windows.Forms.TabPage();
            this.tabControl2 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.ckbSalvar = new System.Windows.Forms.CheckBox();
            this.ckbRetirarAcentos = new System.Windows.Forms.CheckBox();
            this.txtSchemaPath = new System.Windows.Forms.TextBox();
            this.label33 = new System.Windows.Forms.Label();
            this.btnSelectSchema = new System.Windows.Forms.Button();
            this.label32 = new System.Windows.Forms.Label();
            this.cmbVersaoDF = new System.Windows.Forms.ComboBox();
            this.label31 = new System.Windows.Forms.Label();
            this.cmbFormaEmissao = new System.Windows.Forms.ComboBox();
            this.label30 = new System.Windows.Forms.Label();
            this.txtFormatoAlerta = new System.Windows.Forms.TextBox();
            this.ckbExibirErroSchema = new System.Windows.Forms.CheckBox();
            this.ckbAtualizarXML = new System.Windows.Forms.CheckBox();
            this.label17 = new System.Windows.Forms.Label();
            this.cmbModeloDocumento = new System.Windows.Forms.ComboBox();
            this.txtLogs = new System.Windows.Forms.TextBox();
            this.txtCSC = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.txtIdCSC = new System.Windows.Forms.TextBox();
            this.btnSelectLog = new System.Windows.Forms.Button();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.ckbSalvarSOAP = new System.Windows.Forms.CheckBox();
            this.ckbVisualizar = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.nudIntervalos = new System.Windows.Forms.NumericUpDown();
            this.label29 = new System.Windows.Forms.Label();
            this.nudTentativas = new System.Windows.Forms.NumericUpDown();
            this.label28 = new System.Windows.Forms.Label();
            this.nudAguardar = new System.Windows.Forms.NumericUpDown();
            this.label27 = new System.Windows.Forms.Label();
            this.ckbAjustarAut = new System.Windows.Forms.CheckBox();
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
            this.btnObterCertificados = new System.Windows.Forms.Button();
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
            this.txtArqEvento = new System.Windows.Forms.TextBox();
            this.btnArqEvento = new System.Windows.Forms.Button();
            this.label34 = new System.Windows.Forms.Label();
            this.txtArqInu = new System.Windows.Forms.TextBox();
            this.btnArqInu = new System.Windows.Forms.Button();
            this.label35 = new System.Windows.Forms.Label();
            this.txtArqNFe = new System.Windows.Forms.TextBox();
            this.btnArqNFe = new System.Windows.Forms.Button();
            this.label36 = new System.Windows.Forms.Label();
            this.ckbSepararPorModelo = new System.Windows.Forms.CheckBox();
            this.ckbSepararPorCNPJ = new System.Windows.Forms.CheckBox();
            this.ckbSalvaPathEvento = new System.Windows.Forms.CheckBox();
            this.ckbEmissaoPathNFe = new System.Windows.Forms.CheckBox();
            this.ckbAdicionaLiteral = new System.Windows.Forms.CheckBox();
            this.ckbPastaMensal = new System.Windows.Forms.CheckBox();
            this.ckbSalvarArqs = new System.Windows.Forms.CheckBox();
            this.tbpDocumentoAuxiliar = new System.Windows.Forms.TabPage();
            this.groupBox9 = new System.Windows.Forms.GroupBox();
            this.cbbPaginaCodigo = new System.Windows.Forms.ComboBox();
            this.label38 = new System.Windows.Forms.Label();
            this.cbxIgnorarTags = new System.Windows.Forms.CheckBox();
            this.cbxTraduzirTags = new System.Windows.Forms.CheckBox();
            this.cbxCortarPapel = new System.Windows.Forms.CheckBox();
            this.cbxControlePorta = new System.Windows.Forms.CheckBox();
            this.label39 = new System.Windows.Forms.Label();
            this.nudLinhasPular = new System.Windows.Forms.NumericUpDown();
            this.label40 = new System.Windows.Forms.Label();
            this.nudBuffer = new System.Windows.Forms.NumericUpDown();
            this.label41 = new System.Windows.Forms.Label();
            this.nudEspacos = new System.Windows.Forms.NumericUpDown();
            this.label42 = new System.Windows.Forms.Label();
            this.label43 = new System.Windows.Forms.Label();
            this.nudColunas = new System.Windows.Forms.NumericUpDown();
            this.cbbPortas = new System.Windows.Forms.ComboBox();
            this.cbbModelo = new System.Windows.Forms.ComboBox();
            this.label44 = new System.Windows.Forms.Label();
            this.groupBox8 = new System.Windows.Forms.GroupBox();
            this.rdbFortesA4 = new System.Windows.Forms.RadioButton();
            this.rdbEscPos = new System.Windows.Forms.RadioButton();
            this.rdbFortes = new System.Windows.Forms.RadioButton();
            this.groupBox7 = new System.Windows.Forms.GroupBox();
            this.rdbPaisagem = new System.Windows.Forms.RadioButton();
            this.rdbRetrato = new System.Windows.Forms.RadioButton();
            this.txtLogomarca = new System.Windows.Forms.TextBox();
            this.btnLogomarca = new System.Windows.Forms.Button();
            this.label37 = new System.Windows.Forms.Label();
            this.tbpEmail = new System.Windows.Forms.TabPage();
            this.txtMensagem = new System.Windows.Forms.TextBox();
            this.label25 = new System.Windows.Forms.Label();
            this.txtAssunto = new System.Windows.Forms.TextBox();
            this.label24 = new System.Windows.Forms.Label();
            this.groupBox6 = new System.Windows.Forms.GroupBox();
            this.txtSenha = new System.Windows.Forms.TextBox();
            this.label19 = new System.Windows.Forms.Label();
            this.label22 = new System.Windows.Forms.Label();
            this.label20 = new System.Windows.Forms.Label();
            this.txtUsuario = new System.Windows.Forms.TextBox();
            this.txtHost = new System.Windows.Forms.TextBox();
            this.label23 = new System.Windows.Forms.Label();
            this.ckbSSL = new System.Windows.Forms.CheckBox();
            this.label21 = new System.Windows.Forms.Label();
            this.ckbTLS = new System.Windows.Forms.CheckBox();
            this.nudPorta = new System.Windows.Forms.NumericUpDown();
            this.txtNome = new System.Windows.Forms.TextBox();
            this.txtEmail = new System.Windows.Forms.TextBox();
            this.label18 = new System.Windows.Forms.Label();
            this.btnSalvar = new System.Windows.Forms.Button();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.tabControl3 = new System.Windows.Forms.TabControl();
            this.tabPage5 = new System.Windows.Forms.TabPage();
            this.btnOpenSSLInfo = new System.Windows.Forms.Button();
            this.btnSalvarPDF = new System.Windows.Forms.Button();
            this.btnGerarChave = new System.Windows.Forms.Button();
            this.btnLimparLista = new System.Windows.Forms.Button();
            this.btnImprimirPDF = new System.Windows.Forms.Button();
            this.btnEnviarAssincrono = new System.Windows.Forms.Button();
            this.btnEnviarSincrono = new System.Windows.Forms.Button();
            this.btnCarregarXml = new System.Windows.Forms.Button();
            this.btnValidarRegra = new System.Windows.Forms.Button();
            this.btnGerarXml = new System.Windows.Forms.Button();
            this.btnCarregarIni = new System.Windows.Forms.Button();
            this.btnAssinar = new System.Windows.Forms.Button();
            this.btnImprimir = new System.Windows.Forms.Button();
            this.btnEnviarEmail = new System.Windows.Forms.Button();
            this.tabPage6 = new System.Windows.Forms.TabPage();
            this.btnConsultarCadastro = new System.Windows.Forms.Button();
            this.btnStatusServ = new System.Windows.Forms.Button();
            this.btnConsultaXml = new System.Windows.Forms.Button();
            this.btnConsultaChave = new System.Windows.Forms.Button();
            this.btnConsultarRecibo = new System.Windows.Forms.Button();
            this.tabPage7 = new System.Windows.Forms.TabPage();
            this.btnEnviarEvento = new System.Windows.Forms.Button();
            this.btnImprimirEventoPDF = new System.Windows.Forms.Button();
            this.btnLimparListaEvento = new System.Windows.Forms.Button();
            this.btnImprimirEvento = new System.Windows.Forms.Button();
            this.btnEnviarEmailEvento = new System.Windows.Forms.Button();
            this.btnCarregarEvento = new System.Windows.Forms.Button();
            this.btnCancelar = new System.Windows.Forms.Button();
            this.tabPage8 = new System.Windows.Forms.TabPage();
            this.btnImprimirInutilizacaoPDF = new System.Windows.Forms.Button();
            this.btnImprimirInutilizacao = new System.Windows.Forms.Button();
            this.btnInutilizar = new System.Windows.Forms.Button();
            this.tabPage9 = new System.Windows.Forms.TabPage();
            this.btnDFePorUltNSU = new System.Windows.Forms.Button();
            this.btnDFePorNSU = new System.Windows.Forms.Button();
            this.btnDFePorChave = new System.Windows.Forms.Button();
            this.btnCarregarConfiguracoes = new System.Windows.Forms.Button();
            this.btnClasseAltoNivel = new System.Windows.Forms.Button();
            this.tabControl1.SuspendLayout();
            this.tbpConfiguracoes.SuspendLayout();
            this.tabControl2.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudIntervalos)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudTentativas)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudAguardar)).BeginInit();
            this.groupBox5.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudProxyPorta)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudTimeOut)).BeginInit();
            this.groupBox3.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.tabPage4.SuspendLayout();
            this.tbpDocumentoAuxiliar.SuspendLayout();
            this.groupBox9.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudLinhasPular)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudBuffer)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudEspacos)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudColunas)).BeginInit();
            this.groupBox8.SuspendLayout();
            this.groupBox7.SuspendLayout();
            this.tbpEmail.SuspendLayout();
            this.groupBox6.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudPorta)).BeginInit();
            this.groupBox2.SuspendLayout();
            this.tabControl3.SuspendLayout();
            this.tabPage5.SuspendLayout();
            this.tabPage6.SuspendLayout();
            this.tabPage7.SuspendLayout();
            this.tabPage8.SuspendLayout();
            this.tabPage9.SuspendLayout();
            this.SuspendLayout();
            // 
            // rtbRespostas
            // 
            this.rtbRespostas.Dock = System.Windows.Forms.DockStyle.Fill;
            this.rtbRespostas.Location = new System.Drawing.Point(3, 16);
            this.rtbRespostas.Name = "rtbRespostas";
            this.rtbRespostas.Size = new System.Drawing.Size(380, 271);
            this.rtbRespostas.TabIndex = 3;
            this.rtbRespostas.Text = "";
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tbpConfiguracoes);
            this.tabControl1.Controls.Add(this.tbpDocumentoAuxiliar);
            this.tabControl1.Controls.Add(this.tbpEmail);
            this.tabControl1.Location = new System.Drawing.Point(12, 12);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(301, 484);
            this.tabControl1.TabIndex = 4;
            // 
            // tbpConfiguracoes
            // 
            this.tbpConfiguracoes.Controls.Add(this.tabControl2);
            this.tbpConfiguracoes.Location = new System.Drawing.Point(4, 22);
            this.tbpConfiguracoes.Name = "tbpConfiguracoes";
            this.tbpConfiguracoes.Padding = new System.Windows.Forms.Padding(3);
            this.tbpConfiguracoes.Size = new System.Drawing.Size(293, 458);
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
            this.tabControl2.Size = new System.Drawing.Size(287, 452);
            this.tabControl2.TabIndex = 24;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.ckbSalvar);
            this.tabPage1.Controls.Add(this.ckbRetirarAcentos);
            this.tabPage1.Controls.Add(this.txtSchemaPath);
            this.tabPage1.Controls.Add(this.label33);
            this.tabPage1.Controls.Add(this.btnSelectSchema);
            this.tabPage1.Controls.Add(this.label32);
            this.tabPage1.Controls.Add(this.cmbVersaoDF);
            this.tabPage1.Controls.Add(this.label31);
            this.tabPage1.Controls.Add(this.cmbFormaEmissao);
            this.tabPage1.Controls.Add(this.label30);
            this.tabPage1.Controls.Add(this.txtFormatoAlerta);
            this.tabPage1.Controls.Add(this.ckbExibirErroSchema);
            this.tabPage1.Controls.Add(this.ckbAtualizarXML);
            this.tabPage1.Controls.Add(this.label17);
            this.tabPage1.Controls.Add(this.cmbModeloDocumento);
            this.tabPage1.Controls.Add(this.txtLogs);
            this.tabPage1.Controls.Add(this.txtCSC);
            this.tabPage1.Controls.Add(this.label10);
            this.tabPage1.Controls.Add(this.label12);
            this.tabPage1.Controls.Add(this.label11);
            this.tabPage1.Controls.Add(this.txtIdCSC);
            this.tabPage1.Controls.Add(this.btnSelectLog);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(279, 426);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Geral";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // ckbSalvar
            // 
            this.ckbSalvar.AutoSize = true;
            this.ckbSalvar.Location = new System.Drawing.Point(6, 240);
            this.ckbSalvar.Name = "ckbSalvar";
            this.ckbSalvar.Size = new System.Drawing.Size(202, 17);
            this.ckbSalvar.TabIndex = 36;
            this.ckbSalvar.Text = "Salvar Arquivos de Envio e Resposta";
            this.ckbSalvar.UseVisualStyleBackColor = true;
            // 
            // ckbRetirarAcentos
            // 
            this.ckbRetirarAcentos.AutoSize = true;
            this.ckbRetirarAcentos.Location = new System.Drawing.Point(6, 217);
            this.ckbRetirarAcentos.Name = "ckbRetirarAcentos";
            this.ckbRetirarAcentos.Size = new System.Drawing.Size(195, 17);
            this.ckbRetirarAcentos.TabIndex = 35;
            this.ckbRetirarAcentos.Text = "Retirar Acentos dos XMLs enviados";
            this.ckbRetirarAcentos.UseVisualStyleBackColor = true;
            // 
            // txtSchemaPath
            // 
            this.txtSchemaPath.Location = new System.Drawing.Point(6, 315);
            this.txtSchemaPath.Name = "txtSchemaPath";
            this.txtSchemaPath.Size = new System.Drawing.Size(238, 20);
            this.txtSchemaPath.TabIndex = 32;
            // 
            // label33
            // 
            this.label33.AutoSize = true;
            this.label33.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label33.Location = new System.Drawing.Point(3, 299);
            this.label33.Name = "label33";
            this.label33.Size = new System.Drawing.Size(118, 13);
            this.label33.TabIndex = 33;
            this.label33.Text = "Pasta dos Schemas";
            // 
            // btnSelectSchema
            // 
            this.btnSelectSchema.Location = new System.Drawing.Point(243, 314);
            this.btnSelectSchema.Name = "btnSelectSchema";
            this.btnSelectSchema.Size = new System.Drawing.Size(27, 22);
            this.btnSelectSchema.TabIndex = 34;
            this.btnSelectSchema.Text = "...";
            this.btnSelectSchema.UseVisualStyleBackColor = true;
            this.btnSelectSchema.Click += new System.EventHandler(this.BtnSelectSchema_Click);
            // 
            // label32
            // 
            this.label32.AutoSize = true;
            this.label32.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label32.Location = new System.Drawing.Point(3, 174);
            this.label32.Name = "label32";
            this.label32.Size = new System.Drawing.Size(151, 13);
            this.label32.TabIndex = 30;
            this.label32.Text = "Versão Documento Fiscal";
            // 
            // cmbVersaoDF
            // 
            this.cmbVersaoDF.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbVersaoDF.FormattingEnabled = true;
            this.cmbVersaoDF.Location = new System.Drawing.Point(6, 190);
            this.cmbVersaoDF.Name = "cmbVersaoDF";
            this.cmbVersaoDF.Size = new System.Drawing.Size(156, 21);
            this.cmbVersaoDF.TabIndex = 31;
            // 
            // label31
            // 
            this.label31.AutoSize = true;
            this.label31.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label31.Location = new System.Drawing.Point(3, 94);
            this.label31.Name = "label31";
            this.label31.Size = new System.Drawing.Size(109, 13);
            this.label31.TabIndex = 28;
            this.label31.Text = "Forma de Emissão";
            // 
            // cmbFormaEmissao
            // 
            this.cmbFormaEmissao.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbFormaEmissao.FormattingEnabled = true;
            this.cmbFormaEmissao.Location = new System.Drawing.Point(6, 110);
            this.cmbFormaEmissao.Name = "cmbFormaEmissao";
            this.cmbFormaEmissao.Size = new System.Drawing.Size(156, 21);
            this.cmbFormaEmissao.TabIndex = 29;
            // 
            // label30
            // 
            this.label30.AutoSize = true;
            this.label30.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label30.Location = new System.Drawing.Point(3, 55);
            this.label30.Name = "label30";
            this.label30.Size = new System.Drawing.Size(89, 13);
            this.label30.TabIndex = 26;
            this.label30.Text = "Formato Alerta";
            // 
            // txtFormatoAlerta
            // 
            this.txtFormatoAlerta.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFormatoAlerta.Location = new System.Drawing.Point(6, 71);
            this.txtFormatoAlerta.Name = "txtFormatoAlerta";
            this.txtFormatoAlerta.Size = new System.Drawing.Size(267, 20);
            this.txtFormatoAlerta.TabIndex = 27;
            // 
            // ckbExibirErroSchema
            // 
            this.ckbExibirErroSchema.AutoSize = true;
            this.ckbExibirErroSchema.Location = new System.Drawing.Point(6, 29);
            this.ckbExibirErroSchema.Name = "ckbExibirErroSchema";
            this.ckbExibirErroSchema.Size = new System.Drawing.Size(115, 17);
            this.ckbExibirErroSchema.TabIndex = 25;
            this.ckbExibirErroSchema.Text = "Exibir Erro Schema";
            this.ckbExibirErroSchema.UseVisualStyleBackColor = true;
            // 
            // ckbAtualizarXML
            // 
            this.ckbAtualizarXML.AutoSize = true;
            this.ckbAtualizarXML.Location = new System.Drawing.Point(6, 6);
            this.ckbAtualizarXML.Name = "ckbAtualizarXML";
            this.ckbAtualizarXML.Size = new System.Drawing.Size(91, 17);
            this.ckbAtualizarXML.TabIndex = 24;
            this.ckbAtualizarXML.Text = "Atualizar XML";
            this.ckbAtualizarXML.UseVisualStyleBackColor = true;
            // 
            // label17
            // 
            this.label17.AutoSize = true;
            this.label17.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label17.Location = new System.Drawing.Point(3, 134);
            this.label17.Name = "label17";
            this.label17.Size = new System.Drawing.Size(153, 13);
            this.label17.TabIndex = 22;
            this.label17.Text = "Modelo Documento Fiscal";
            // 
            // cmbModeloDocumento
            // 
            this.cmbModeloDocumento.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbModeloDocumento.FormattingEnabled = true;
            this.cmbModeloDocumento.Location = new System.Drawing.Point(6, 150);
            this.cmbModeloDocumento.Name = "cmbModeloDocumento";
            this.cmbModeloDocumento.Size = new System.Drawing.Size(156, 21);
            this.cmbModeloDocumento.TabIndex = 23;
            // 
            // txtLogs
            // 
            this.txtLogs.Location = new System.Drawing.Point(6, 276);
            this.txtLogs.Name = "txtLogs";
            this.txtLogs.Size = new System.Drawing.Size(238, 20);
            this.txtLogs.TabIndex = 13;
            // 
            // txtCSC
            // 
            this.txtCSC.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCSC.Location = new System.Drawing.Point(6, 398);
            this.txtCSC.Name = "txtCSC";
            this.txtCSC.Size = new System.Drawing.Size(231, 20);
            this.txtCSC.TabIndex = 21;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label10.Location = new System.Drawing.Point(3, 260);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(94, 13);
            this.label10.TabIndex = 14;
            this.label10.Text = "Pasta dos Logs";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label12.Location = new System.Drawing.Point(3, 382);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(202, 13);
            this.label12.TabIndex = 20;
            this.label12.Text = "Token/CSC (Somente para NFC-e)";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label11.Location = new System.Drawing.Point(3, 343);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(224, 13);
            this.label11.TabIndex = 18;
            this.label11.Text = "IdToken/IdCSC (Somente para NFC-e)";
            // 
            // txtIdCSC
            // 
            this.txtIdCSC.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtIdCSC.Location = new System.Drawing.Point(6, 359);
            this.txtIdCSC.Name = "txtIdCSC";
            this.txtIdCSC.Size = new System.Drawing.Size(231, 20);
            this.txtIdCSC.TabIndex = 19;
            // 
            // btnSelectLog
            // 
            this.btnSelectLog.Location = new System.Drawing.Point(243, 275);
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
            this.tabPage2.Controls.Add(this.groupBox1);
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
            this.tabPage2.Size = new System.Drawing.Size(279, 426);
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
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.nudIntervalos);
            this.groupBox1.Controls.Add(this.label29);
            this.groupBox1.Controls.Add(this.nudTentativas);
            this.groupBox1.Controls.Add(this.label28);
            this.groupBox1.Controls.Add(this.nudAguardar);
            this.groupBox1.Controls.Add(this.label27);
            this.groupBox1.Controls.Add(this.ckbAjustarAut);
            this.groupBox1.Location = new System.Drawing.Point(6, 146);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(267, 82);
            this.groupBox1.TabIndex = 15;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Retorno de Envio";
            // 
            // nudIntervalos
            // 
            this.nudIntervalos.Location = new System.Drawing.Point(181, 56);
            this.nudIntervalos.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.nudIntervalos.Name = "nudIntervalos";
            this.nudIntervalos.Size = new System.Drawing.Size(80, 20);
            this.nudIntervalos.TabIndex = 13;
            // 
            // label29
            // 
            this.label29.AutoSize = true;
            this.label29.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label29.Location = new System.Drawing.Point(178, 39);
            this.label29.Name = "label29";
            this.label29.Size = new System.Drawing.Size(57, 13);
            this.label29.TabIndex = 12;
            this.label29.Text = "Intervalo";
            // 
            // nudTentativas
            // 
            this.nudTentativas.Location = new System.Drawing.Point(92, 55);
            this.nudTentativas.Name = "nudTentativas";
            this.nudTentativas.Size = new System.Drawing.Size(83, 20);
            this.nudTentativas.TabIndex = 11;
            // 
            // label28
            // 
            this.label28.AutoSize = true;
            this.label28.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label28.Location = new System.Drawing.Point(89, 39);
            this.label28.Name = "label28";
            this.label28.Size = new System.Drawing.Size(67, 13);
            this.label28.TabIndex = 10;
            this.label28.Text = "Tentativas";
            // 
            // nudAguardar
            // 
            this.nudAguardar.Location = new System.Drawing.Point(6, 56);
            this.nudAguardar.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.nudAguardar.Name = "nudAguardar";
            this.nudAguardar.Size = new System.Drawing.Size(80, 20);
            this.nudAguardar.TabIndex = 9;
            // 
            // label27
            // 
            this.label27.AutoSize = true;
            this.label27.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label27.Location = new System.Drawing.Point(3, 39);
            this.label27.Name = "label27";
            this.label27.Size = new System.Drawing.Size(58, 13);
            this.label27.TabIndex = 8;
            this.label27.Text = "Aguardar";
            // 
            // ckbAjustarAut
            // 
            this.ckbAjustarAut.AutoSize = true;
            this.ckbAjustarAut.Location = new System.Drawing.Point(6, 19);
            this.ckbAjustarAut.Name = "ckbAjustarAut";
            this.ckbAjustarAut.Size = new System.Drawing.Size(226, 17);
            this.ckbAjustarAut.TabIndex = 0;
            this.ckbAjustarAut.Text = "Ajustar Automaticamente prop. \"Aguardar\"";
            this.ckbAjustarAut.UseVisualStyleBackColor = true;
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
            this.groupBox5.Location = new System.Drawing.Point(6, 234);
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
            this.tabPage3.Controls.Add(this.btnObterCertificados);
            this.tabPage3.Controls.Add(this.groupBox4);
            this.tabPage3.Controls.Add(this.cmbXmlSign);
            this.tabPage3.Controls.Add(this.label6);
            this.tabPage3.Controls.Add(this.cmbHttp);
            this.tabPage3.Controls.Add(this.label5);
            this.tabPage3.Controls.Add(this.cmbCrypt);
            this.tabPage3.Controls.Add(this.label4);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Size = new System.Drawing.Size(279, 426);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "Certificados";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // btnObterCertificados
            // 
            this.btnObterCertificados.Location = new System.Drawing.Point(6, 315);
            this.btnObterCertificados.Name = "btnObterCertificados";
            this.btnObterCertificados.Size = new System.Drawing.Size(118, 23);
            this.btnObterCertificados.TabIndex = 20;
            this.btnObterCertificados.Text = "Obter Certificados";
            this.btnObterCertificados.UseVisualStyleBackColor = true;
            this.btnObterCertificados.Click += new System.EventHandler(this.btnObterCertificados_Click);
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
            this.btnSelecionarCertificado.Click += new System.EventHandler(this.BtnSelecionarCertificado_Click);
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
            this.tabPage4.Controls.Add(this.txtArqEvento);
            this.tabPage4.Controls.Add(this.btnArqEvento);
            this.tabPage4.Controls.Add(this.label34);
            this.tabPage4.Controls.Add(this.txtArqInu);
            this.tabPage4.Controls.Add(this.btnArqInu);
            this.tabPage4.Controls.Add(this.label35);
            this.tabPage4.Controls.Add(this.txtArqNFe);
            this.tabPage4.Controls.Add(this.btnArqNFe);
            this.tabPage4.Controls.Add(this.label36);
            this.tabPage4.Controls.Add(this.ckbSepararPorModelo);
            this.tabPage4.Controls.Add(this.ckbSepararPorCNPJ);
            this.tabPage4.Controls.Add(this.ckbSalvaPathEvento);
            this.tabPage4.Controls.Add(this.ckbEmissaoPathNFe);
            this.tabPage4.Controls.Add(this.ckbAdicionaLiteral);
            this.tabPage4.Controls.Add(this.ckbPastaMensal);
            this.tabPage4.Controls.Add(this.ckbSalvarArqs);
            this.tabPage4.Location = new System.Drawing.Point(4, 22);
            this.tabPage4.Name = "tabPage4";
            this.tabPage4.Size = new System.Drawing.Size(279, 426);
            this.tabPage4.TabIndex = 3;
            this.tabPage4.Text = "Arquivos";
            this.tabPage4.UseVisualStyleBackColor = true;
            // 
            // txtArqEvento
            // 
            this.txtArqEvento.Location = new System.Drawing.Point(3, 258);
            this.txtArqEvento.Name = "txtArqEvento";
            this.txtArqEvento.Size = new System.Drawing.Size(248, 20);
            this.txtArqEvento.TabIndex = 31;
            // 
            // btnArqEvento
            // 
            this.btnArqEvento.Location = new System.Drawing.Point(249, 257);
            this.btnArqEvento.Name = "btnArqEvento";
            this.btnArqEvento.Size = new System.Drawing.Size(27, 22);
            this.btnArqEvento.TabIndex = 33;
            this.btnArqEvento.Text = "...";
            this.btnArqEvento.UseVisualStyleBackColor = true;
            this.btnArqEvento.Click += new System.EventHandler(this.btnArqEvento_Click);
            // 
            // label34
            // 
            this.label34.AutoSize = true;
            this.label34.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label34.Location = new System.Drawing.Point(3, 242);
            this.label34.Name = "label34";
            this.label34.Size = new System.Drawing.Size(136, 13);
            this.label34.TabIndex = 32;
            this.label34.Text = "Pasta Arquivos Evento";
            // 
            // txtArqInu
            // 
            this.txtArqInu.Location = new System.Drawing.Point(3, 219);
            this.txtArqInu.Name = "txtArqInu";
            this.txtArqInu.Size = new System.Drawing.Size(248, 20);
            this.txtArqInu.TabIndex = 28;
            // 
            // btnArqInu
            // 
            this.btnArqInu.Location = new System.Drawing.Point(249, 218);
            this.btnArqInu.Name = "btnArqInu";
            this.btnArqInu.Size = new System.Drawing.Size(27, 22);
            this.btnArqInu.TabIndex = 30;
            this.btnArqInu.Text = "...";
            this.btnArqInu.UseVisualStyleBackColor = true;
            this.btnArqInu.Click += new System.EventHandler(this.btnArqInu_Click);
            // 
            // label35
            // 
            this.label35.AutoSize = true;
            this.label35.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label35.Location = new System.Drawing.Point(3, 203);
            this.label35.Name = "label35";
            this.label35.Size = new System.Drawing.Size(161, 13);
            this.label35.TabIndex = 29;
            this.label35.Text = "Pasta Arquivos Inutilização";
            // 
            // txtArqNFe
            // 
            this.txtArqNFe.Location = new System.Drawing.Point(3, 180);
            this.txtArqNFe.Name = "txtArqNFe";
            this.txtArqNFe.Size = new System.Drawing.Size(248, 20);
            this.txtArqNFe.TabIndex = 25;
            // 
            // btnArqNFe
            // 
            this.btnArqNFe.Location = new System.Drawing.Point(249, 179);
            this.btnArqNFe.Name = "btnArqNFe";
            this.btnArqNFe.Size = new System.Drawing.Size(27, 22);
            this.btnArqNFe.TabIndex = 27;
            this.btnArqNFe.Text = "...";
            this.btnArqNFe.UseVisualStyleBackColor = true;
            this.btnArqNFe.Click += new System.EventHandler(this.btnArqNFe_Click);
            // 
            // label36
            // 
            this.label36.AutoSize = true;
            this.label36.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label36.Location = new System.Drawing.Point(3, 164);
            this.label36.Name = "label36";
            this.label36.Size = new System.Drawing.Size(119, 13);
            this.label36.TabIndex = 26;
            this.label36.Text = "Pasta Arquivos NFe";
            // 
            // ckbSepararPorModelo
            // 
            this.ckbSepararPorModelo.AutoSize = true;
            this.ckbSepararPorModelo.Location = new System.Drawing.Point(6, 144);
            this.ckbSepararPorModelo.Name = "ckbSepararPorModelo";
            this.ckbSepararPorModelo.Size = new System.Drawing.Size(221, 17);
            this.ckbSepararPorModelo.TabIndex = 13;
            this.ckbSepararPorModelo.Text = "Separar Arqs pelo Modelo do Documento";
            this.ckbSepararPorModelo.UseVisualStyleBackColor = true;
            // 
            // ckbSepararPorCNPJ
            // 
            this.ckbSepararPorCNPJ.AutoSize = true;
            this.ckbSepararPorCNPJ.Location = new System.Drawing.Point(6, 121);
            this.ckbSepararPorCNPJ.Name = "ckbSepararPorCNPJ";
            this.ckbSepararPorCNPJ.Size = new System.Drawing.Size(208, 17);
            this.ckbSepararPorCNPJ.TabIndex = 12;
            this.ckbSepararPorCNPJ.Text = "Separar Arqs pelo CNPJ do Certificado";
            this.ckbSepararPorCNPJ.UseVisualStyleBackColor = true;
            // 
            // ckbSalvaPathEvento
            // 
            this.ckbSalvaPathEvento.AutoSize = true;
            this.ckbSalvaPathEvento.Location = new System.Drawing.Point(6, 97);
            this.ckbSalvaPathEvento.Name = "ckbSalvaPathEvento";
            this.ckbSalvaPathEvento.Size = new System.Drawing.Size(157, 17);
            this.ckbSalvaPathEvento.TabIndex = 11;
            this.ckbSalvaPathEvento.Text = "Salvar Arquivos de Eventos";
            this.ckbSalvaPathEvento.UseVisualStyleBackColor = true;
            // 
            // ckbEmissaoPathNFe
            // 
            this.ckbEmissaoPathNFe.AutoSize = true;
            this.ckbEmissaoPathNFe.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ckbEmissaoPathNFe.Location = new System.Drawing.Point(6, 75);
            this.ckbEmissaoPathNFe.Name = "ckbEmissaoPathNFe";
            this.ckbEmissaoPathNFe.Size = new System.Drawing.Size(255, 17);
            this.ckbEmissaoPathNFe.TabIndex = 10;
            this.ckbEmissaoPathNFe.Text = "Salvar Documento pelo campo Data de Emissão";
            this.ckbEmissaoPathNFe.UseVisualStyleBackColor = true;
            // 
            // ckbAdicionaLiteral
            // 
            this.ckbAdicionaLiteral.AutoSize = true;
            this.ckbAdicionaLiteral.Location = new System.Drawing.Point(6, 52);
            this.ckbAdicionaLiteral.Name = "ckbAdicionaLiteral";
            this.ckbAdicionaLiteral.Size = new System.Drawing.Size(199, 17);
            this.ckbAdicionaLiteral.TabIndex = 9;
            this.ckbAdicionaLiteral.Text = "Adicionar Literal no nome das pastas";
            this.ckbAdicionaLiteral.UseVisualStyleBackColor = true;
            // 
            // ckbPastaMensal
            // 
            this.ckbPastaMensal.AutoSize = true;
            this.ckbPastaMensal.Location = new System.Drawing.Point(6, 29);
            this.ckbPastaMensal.Name = "ckbPastaMensal";
            this.ckbPastaMensal.Size = new System.Drawing.Size(148, 17);
            this.ckbPastaMensal.TabIndex = 8;
            this.ckbPastaMensal.Text = "Criar Pastas Mensalmente";
            this.ckbPastaMensal.UseVisualStyleBackColor = true;
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
            // tbpDocumentoAuxiliar
            // 
            this.tbpDocumentoAuxiliar.Controls.Add(this.groupBox9);
            this.tbpDocumentoAuxiliar.Controls.Add(this.groupBox8);
            this.tbpDocumentoAuxiliar.Controls.Add(this.groupBox7);
            this.tbpDocumentoAuxiliar.Controls.Add(this.txtLogomarca);
            this.tbpDocumentoAuxiliar.Controls.Add(this.btnLogomarca);
            this.tbpDocumentoAuxiliar.Controls.Add(this.label37);
            this.tbpDocumentoAuxiliar.Location = new System.Drawing.Point(4, 22);
            this.tbpDocumentoAuxiliar.Name = "tbpDocumentoAuxiliar";
            this.tbpDocumentoAuxiliar.Padding = new System.Windows.Forms.Padding(3);
            this.tbpDocumentoAuxiliar.Size = new System.Drawing.Size(293, 458);
            this.tbpDocumentoAuxiliar.TabIndex = 1;
            this.tbpDocumentoAuxiliar.Text = "Documento Auxiliar";
            this.tbpDocumentoAuxiliar.UseVisualStyleBackColor = true;
            // 
            // groupBox9
            // 
            this.groupBox9.Controls.Add(this.cbbPaginaCodigo);
            this.groupBox9.Controls.Add(this.label38);
            this.groupBox9.Controls.Add(this.cbxIgnorarTags);
            this.groupBox9.Controls.Add(this.cbxTraduzirTags);
            this.groupBox9.Controls.Add(this.cbxCortarPapel);
            this.groupBox9.Controls.Add(this.cbxControlePorta);
            this.groupBox9.Controls.Add(this.label39);
            this.groupBox9.Controls.Add(this.nudLinhasPular);
            this.groupBox9.Controls.Add(this.label40);
            this.groupBox9.Controls.Add(this.nudBuffer);
            this.groupBox9.Controls.Add(this.label41);
            this.groupBox9.Controls.Add(this.nudEspacos);
            this.groupBox9.Controls.Add(this.label42);
            this.groupBox9.Controls.Add(this.label43);
            this.groupBox9.Controls.Add(this.nudColunas);
            this.groupBox9.Controls.Add(this.cbbPortas);
            this.groupBox9.Controls.Add(this.cbbModelo);
            this.groupBox9.Controls.Add(this.label44);
            this.groupBox9.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold);
            this.groupBox9.Location = new System.Drawing.Point(6, 152);
            this.groupBox9.Name = "groupBox9";
            this.groupBox9.Size = new System.Drawing.Size(281, 188);
            this.groupBox9.TabIndex = 78;
            this.groupBox9.TabStop = false;
            this.groupBox9.Text = "Pos Printer";
            // 
            // cbbPaginaCodigo
            // 
            this.cbbPaginaCodigo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbbPaginaCodigo.FormattingEnabled = true;
            this.cbbPaginaCodigo.Location = new System.Drawing.Point(155, 32);
            this.cbbPaginaCodigo.Name = "cbbPaginaCodigo";
            this.cbbPaginaCodigo.Size = new System.Drawing.Size(120, 21);
            this.cbbPaginaCodigo.TabIndex = 35;
            // 
            // label38
            // 
            this.label38.AutoSize = true;
            this.label38.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label38.Location = new System.Drawing.Point(152, 16);
            this.label38.Name = "label38";
            this.label38.Size = new System.Drawing.Size(65, 13);
            this.label38.TabIndex = 34;
            this.label38.Text = "Pag. Código";
            // 
            // cbxIgnorarTags
            // 
            this.cbxIgnorarTags.AutoSize = true;
            this.cbxIgnorarTags.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.cbxIgnorarTags.Location = new System.Drawing.Point(171, 162);
            this.cbxIgnorarTags.Name = "cbxIgnorarTags";
            this.cbxIgnorarTags.Size = new System.Drawing.Size(86, 17);
            this.cbxIgnorarTags.TabIndex = 33;
            this.cbxIgnorarTags.Text = "Ignorar Tags";
            this.cbxIgnorarTags.UseVisualStyleBackColor = true;
            // 
            // cbxTraduzirTags
            // 
            this.cbxTraduzirTags.AutoSize = true;
            this.cbxTraduzirTags.Checked = true;
            this.cbxTraduzirTags.CheckState = System.Windows.Forms.CheckState.Checked;
            this.cbxTraduzirTags.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.cbxTraduzirTags.Location = new System.Drawing.Point(171, 139);
            this.cbxTraduzirTags.Name = "cbxTraduzirTags";
            this.cbxTraduzirTags.Size = new System.Drawing.Size(91, 17);
            this.cbxTraduzirTags.TabIndex = 32;
            this.cbxTraduzirTags.Text = "Traduzir Tags";
            this.cbxTraduzirTags.UseVisualStyleBackColor = true;
            // 
            // cbxCortarPapel
            // 
            this.cbxCortarPapel.AutoSize = true;
            this.cbxCortarPapel.Checked = true;
            this.cbxCortarPapel.CheckState = System.Windows.Forms.CheckState.Checked;
            this.cbxCortarPapel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.cbxCortarPapel.Location = new System.Drawing.Point(9, 162);
            this.cbxCortarPapel.Name = "cbxCortarPapel";
            this.cbxCortarPapel.Size = new System.Drawing.Size(84, 17);
            this.cbxCortarPapel.TabIndex = 31;
            this.cbxCortarPapel.Text = "Cortar Papel";
            this.cbxCortarPapel.UseVisualStyleBackColor = true;
            // 
            // cbxControlePorta
            // 
            this.cbxControlePorta.AutoSize = true;
            this.cbxControlePorta.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.cbxControlePorta.Location = new System.Drawing.Point(9, 139);
            this.cbxControlePorta.Name = "cbxControlePorta";
            this.cbxControlePorta.Size = new System.Drawing.Size(93, 17);
            this.cbxControlePorta.TabIndex = 30;
            this.cbxControlePorta.Text = "Controle Porta";
            this.cbxControlePorta.UseVisualStyleBackColor = true;
            // 
            // label39
            // 
            this.label39.AutoSize = true;
            this.label39.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label39.Location = new System.Drawing.Point(198, 97);
            this.label39.Name = "label39";
            this.label39.Size = new System.Drawing.Size(65, 13);
            this.label39.TabIndex = 29;
            this.label39.Text = "Linhas Pular";
            // 
            // nudLinhasPular
            // 
            this.nudLinhasPular.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.nudLinhasPular.Location = new System.Drawing.Point(200, 112);
            this.nudLinhasPular.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudLinhasPular.Name = "nudLinhasPular";
            this.nudLinhasPular.Size = new System.Drawing.Size(75, 20);
            this.nudLinhasPular.TabIndex = 28;
            // 
            // label40
            // 
            this.label40.AutoSize = true;
            this.label40.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label40.Location = new System.Drawing.Point(122, 96);
            this.label40.Name = "label40";
            this.label40.Size = new System.Drawing.Size(35, 13);
            this.label40.TabIndex = 27;
            this.label40.Text = "Buffer";
            // 
            // nudBuffer
            // 
            this.nudBuffer.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.nudBuffer.Location = new System.Drawing.Point(122, 112);
            this.nudBuffer.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudBuffer.Name = "nudBuffer";
            this.nudBuffer.Size = new System.Drawing.Size(73, 20);
            this.nudBuffer.TabIndex = 26;
            // 
            // label41
            // 
            this.label41.AutoSize = true;
            this.label41.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label41.Location = new System.Drawing.Point(61, 97);
            this.label41.Name = "label41";
            this.label41.Size = new System.Drawing.Size(48, 13);
            this.label41.TabIndex = 25;
            this.label41.Text = "Espaços";
            // 
            // nudEspacos
            // 
            this.nudEspacos.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.nudEspacos.Location = new System.Drawing.Point(61, 113);
            this.nudEspacos.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudEspacos.Name = "nudEspacos";
            this.nudEspacos.Size = new System.Drawing.Size(55, 20);
            this.nudEspacos.TabIndex = 24;
            // 
            // label42
            // 
            this.label42.AutoSize = true;
            this.label42.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label42.Location = new System.Drawing.Point(6, 96);
            this.label42.Name = "label42";
            this.label42.Size = new System.Drawing.Size(45, 13);
            this.label42.TabIndex = 23;
            this.label42.Text = "Colunas";
            // 
            // label43
            // 
            this.label43.AutoSize = true;
            this.label43.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label43.Location = new System.Drawing.Point(6, 56);
            this.label43.Name = "label43";
            this.label43.Size = new System.Drawing.Size(32, 13);
            this.label43.TabIndex = 22;
            this.label43.Text = "Porta";
            // 
            // nudColunas
            // 
            this.nudColunas.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.nudColunas.Location = new System.Drawing.Point(6, 112);
            this.nudColunas.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudColunas.Name = "nudColunas";
            this.nudColunas.Size = new System.Drawing.Size(52, 20);
            this.nudColunas.TabIndex = 18;
            // 
            // cbbPortas
            // 
            this.cbbPortas.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.cbbPortas.FormattingEnabled = true;
            this.cbbPortas.Location = new System.Drawing.Point(6, 72);
            this.cbbPortas.Name = "cbbPortas";
            this.cbbPortas.Size = new System.Drawing.Size(269, 21);
            this.cbbPortas.TabIndex = 21;
            // 
            // cbbModelo
            // 
            this.cbbModelo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbbModelo.FormattingEnabled = true;
            this.cbbModelo.Location = new System.Drawing.Point(6, 32);
            this.cbbModelo.Name = "cbbModelo";
            this.cbbModelo.Size = new System.Drawing.Size(143, 21);
            this.cbbModelo.TabIndex = 20;
            // 
            // label44
            // 
            this.label44.AutoSize = true;
            this.label44.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.label44.Location = new System.Drawing.Point(3, 16);
            this.label44.Name = "label44";
            this.label44.Size = new System.Drawing.Size(42, 13);
            this.label44.TabIndex = 19;
            this.label44.Text = "Modelo";
            // 
            // groupBox8
            // 
            this.groupBox8.Controls.Add(this.rdbFortesA4);
            this.groupBox8.Controls.Add(this.rdbEscPos);
            this.groupBox8.Controls.Add(this.rdbFortes);
            this.groupBox8.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox8.Location = new System.Drawing.Point(6, 103);
            this.groupBox8.Name = "groupBox8";
            this.groupBox8.Size = new System.Drawing.Size(281, 43);
            this.groupBox8.TabIndex = 24;
            this.groupBox8.TabStop = false;
            this.groupBox8.Text = "DANFCe";
            // 
            // rdbFortesA4
            // 
            this.rdbFortesA4.AutoSize = true;
            this.rdbFortesA4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbFortesA4.Location = new System.Drawing.Point(184, 19);
            this.rdbFortesA4.Name = "rdbFortesA4";
            this.rdbFortesA4.Size = new System.Drawing.Size(70, 17);
            this.rdbFortesA4.TabIndex = 2;
            this.rdbFortesA4.TabStop = true;
            this.rdbFortesA4.Text = "Fortes A4";
            this.rdbFortesA4.UseVisualStyleBackColor = true;
            // 
            // rdbEscPos
            // 
            this.rdbEscPos.AutoSize = true;
            this.rdbEscPos.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbEscPos.Location = new System.Drawing.Point(97, 19);
            this.rdbEscPos.Name = "rdbEscPos";
            this.rdbEscPos.Size = new System.Drawing.Size(61, 17);
            this.rdbEscPos.TabIndex = 1;
            this.rdbEscPos.TabStop = true;
            this.rdbEscPos.Text = "EscPos";
            this.rdbEscPos.UseVisualStyleBackColor = true;
            // 
            // rdbFortes
            // 
            this.rdbFortes.AutoSize = true;
            this.rdbFortes.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbFortes.Location = new System.Drawing.Point(6, 19);
            this.rdbFortes.Name = "rdbFortes";
            this.rdbFortes.Size = new System.Drawing.Size(54, 17);
            this.rdbFortes.TabIndex = 0;
            this.rdbFortes.TabStop = true;
            this.rdbFortes.Text = "Fortes";
            this.rdbFortes.UseVisualStyleBackColor = true;
            // 
            // groupBox7
            // 
            this.groupBox7.Controls.Add(this.rdbPaisagem);
            this.groupBox7.Controls.Add(this.rdbRetrato);
            this.groupBox7.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox7.Location = new System.Drawing.Point(6, 51);
            this.groupBox7.Name = "groupBox7";
            this.groupBox7.Size = new System.Drawing.Size(281, 46);
            this.groupBox7.TabIndex = 23;
            this.groupBox7.TabStop = false;
            this.groupBox7.Text = "DANFe";
            // 
            // rdbPaisagem
            // 
            this.rdbPaisagem.AutoSize = true;
            this.rdbPaisagem.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbPaisagem.Location = new System.Drawing.Point(184, 19);
            this.rdbPaisagem.Name = "rdbPaisagem";
            this.rdbPaisagem.Size = new System.Drawing.Size(71, 17);
            this.rdbPaisagem.TabIndex = 1;
            this.rdbPaisagem.TabStop = true;
            this.rdbPaisagem.Text = "Paisagem";
            this.rdbPaisagem.UseVisualStyleBackColor = true;
            // 
            // rdbRetrato
            // 
            this.rdbRetrato.AutoSize = true;
            this.rdbRetrato.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rdbRetrato.Location = new System.Drawing.Point(6, 19);
            this.rdbRetrato.Name = "rdbRetrato";
            this.rdbRetrato.Size = new System.Drawing.Size(60, 17);
            this.rdbRetrato.TabIndex = 0;
            this.rdbRetrato.TabStop = true;
            this.rdbRetrato.Text = "Retrato";
            this.rdbRetrato.UseVisualStyleBackColor = true;
            // 
            // txtLogomarca
            // 
            this.txtLogomarca.Location = new System.Drawing.Point(6, 25);
            this.txtLogomarca.Name = "txtLogomarca";
            this.txtLogomarca.Size = new System.Drawing.Size(258, 20);
            this.txtLogomarca.TabIndex = 20;
            // 
            // btnLogomarca
            // 
            this.btnLogomarca.Location = new System.Drawing.Point(263, 24);
            this.btnLogomarca.Name = "btnLogomarca";
            this.btnLogomarca.Size = new System.Drawing.Size(27, 22);
            this.btnLogomarca.TabIndex = 22;
            this.btnLogomarca.Text = "...";
            this.btnLogomarca.UseVisualStyleBackColor = true;
            this.btnLogomarca.Click += new System.EventHandler(this.btnLogomarca_Click);
            // 
            // label37
            // 
            this.label37.AutoSize = true;
            this.label37.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label37.Location = new System.Drawing.Point(6, 9);
            this.label37.Name = "label37";
            this.label37.Size = new System.Drawing.Size(69, 13);
            this.label37.TabIndex = 21;
            this.label37.Text = "Logomarca";
            // 
            // tbpEmail
            // 
            this.tbpEmail.Controls.Add(this.txtMensagem);
            this.tbpEmail.Controls.Add(this.label25);
            this.tbpEmail.Controls.Add(this.txtAssunto);
            this.tbpEmail.Controls.Add(this.label24);
            this.tbpEmail.Controls.Add(this.groupBox6);
            this.tbpEmail.Location = new System.Drawing.Point(4, 22);
            this.tbpEmail.Name = "tbpEmail";
            this.tbpEmail.Padding = new System.Windows.Forms.Padding(3);
            this.tbpEmail.Size = new System.Drawing.Size(293, 458);
            this.tbpEmail.TabIndex = 3;
            this.tbpEmail.Text = "Email";
            this.tbpEmail.UseVisualStyleBackColor = true;
            // 
            // txtMensagem
            // 
            this.txtMensagem.Location = new System.Drawing.Point(6, 327);
            this.txtMensagem.Multiline = true;
            this.txtMensagem.Name = "txtMensagem";
            this.txtMensagem.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.txtMensagem.Size = new System.Drawing.Size(281, 80);
            this.txtMensagem.TabIndex = 34;
            this.txtMensagem.WordWrap = false;
            // 
            // label25
            // 
            this.label25.AutoSize = true;
            this.label25.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label25.Location = new System.Drawing.Point(3, 311);
            this.label25.Name = "label25";
            this.label25.Size = new System.Drawing.Size(67, 13);
            this.label25.TabIndex = 35;
            this.label25.Text = "Mensagem";
            // 
            // txtAssunto
            // 
            this.txtAssunto.Location = new System.Drawing.Point(6, 288);
            this.txtAssunto.Name = "txtAssunto";
            this.txtAssunto.Size = new System.Drawing.Size(281, 20);
            this.txtAssunto.TabIndex = 32;
            // 
            // label24
            // 
            this.label24.AutoSize = true;
            this.label24.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label24.Location = new System.Drawing.Point(3, 272);
            this.label24.Name = "label24";
            this.label24.Size = new System.Drawing.Size(52, 13);
            this.label24.TabIndex = 33;
            this.label24.Text = "Assunto";
            // 
            // groupBox6
            // 
            this.groupBox6.Controls.Add(this.txtSenha);
            this.groupBox6.Controls.Add(this.label19);
            this.groupBox6.Controls.Add(this.label22);
            this.groupBox6.Controls.Add(this.label20);
            this.groupBox6.Controls.Add(this.txtUsuario);
            this.groupBox6.Controls.Add(this.txtHost);
            this.groupBox6.Controls.Add(this.label23);
            this.groupBox6.Controls.Add(this.ckbSSL);
            this.groupBox6.Controls.Add(this.label21);
            this.groupBox6.Controls.Add(this.ckbTLS);
            this.groupBox6.Controls.Add(this.nudPorta);
            this.groupBox6.Controls.Add(this.txtNome);
            this.groupBox6.Controls.Add(this.txtEmail);
            this.groupBox6.Controls.Add(this.label18);
            this.groupBox6.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox6.Location = new System.Drawing.Point(6, 6);
            this.groupBox6.Name = "groupBox6";
            this.groupBox6.Size = new System.Drawing.Size(281, 263);
            this.groupBox6.TabIndex = 31;
            this.groupBox6.TabStop = false;
            this.groupBox6.Text = "Configurações";
            // 
            // txtSenha
            // 
            this.txtSenha.Location = new System.Drawing.Point(9, 227);
            this.txtSenha.Name = "txtSenha";
            this.txtSenha.PasswordChar = '*';
            this.txtSenha.Size = new System.Drawing.Size(266, 20);
            this.txtSenha.TabIndex = 28;
            // 
            // label19
            // 
            this.label19.AutoSize = true;
            this.label19.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label19.Location = new System.Drawing.Point(6, 16);
            this.label19.Name = "label19";
            this.label19.Size = new System.Drawing.Size(39, 13);
            this.label19.TabIndex = 23;
            this.label19.Text = "Nome";
            // 
            // label22
            // 
            this.label22.AutoSize = true;
            this.label22.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label22.Location = new System.Drawing.Point(6, 211);
            this.label22.Name = "label22";
            this.label22.Size = new System.Drawing.Size(43, 13);
            this.label22.TabIndex = 30;
            this.label22.Text = "Senha";
            // 
            // label20
            // 
            this.label20.AutoSize = true;
            this.label20.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label20.Location = new System.Drawing.Point(6, 94);
            this.label20.Name = "label20";
            this.label20.Size = new System.Drawing.Size(71, 13);
            this.label20.TabIndex = 19;
            this.label20.Text = "Host SMTP";
            // 
            // txtUsuario
            // 
            this.txtUsuario.Location = new System.Drawing.Point(9, 188);
            this.txtUsuario.Name = "txtUsuario";
            this.txtUsuario.Size = new System.Drawing.Size(266, 20);
            this.txtUsuario.TabIndex = 27;
            // 
            // txtHost
            // 
            this.txtHost.Location = new System.Drawing.Point(9, 110);
            this.txtHost.Name = "txtHost";
            this.txtHost.Size = new System.Drawing.Size(266, 20);
            this.txtHost.TabIndex = 20;
            // 
            // label23
            // 
            this.label23.AutoSize = true;
            this.label23.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label23.Location = new System.Drawing.Point(6, 172);
            this.label23.Name = "label23";
            this.label23.Size = new System.Drawing.Size(50, 13);
            this.label23.TabIndex = 29;
            this.label23.Text = "Usuário";
            // 
            // ckbSSL
            // 
            this.ckbSSL.AutoSize = true;
            this.ckbSSL.Location = new System.Drawing.Point(79, 136);
            this.ckbSSL.Name = "ckbSSL";
            this.ckbSSL.Size = new System.Drawing.Size(49, 17);
            this.ckbSSL.TabIndex = 21;
            this.ckbSSL.Text = "SSL";
            this.ckbSSL.UseVisualStyleBackColor = true;
            // 
            // label21
            // 
            this.label21.AutoSize = true;
            this.label21.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label21.Location = new System.Drawing.Point(9, 133);
            this.label21.Name = "label21";
            this.label21.Size = new System.Drawing.Size(37, 13);
            this.label21.TabIndex = 26;
            this.label21.Text = "Porta";
            // 
            // ckbTLS
            // 
            this.ckbTLS.AutoSize = true;
            this.ckbTLS.Location = new System.Drawing.Point(79, 152);
            this.ckbTLS.Name = "ckbTLS";
            this.ckbTLS.Size = new System.Drawing.Size(49, 17);
            this.ckbTLS.TabIndex = 22;
            this.ckbTLS.Text = "TLS";
            this.ckbTLS.UseVisualStyleBackColor = true;
            // 
            // nudPorta
            // 
            this.nudPorta.Location = new System.Drawing.Point(12, 149);
            this.nudPorta.Maximum = new decimal(new int[] {
            9999,
            0,
            0,
            0});
            this.nudPorta.Name = "nudPorta";
            this.nudPorta.Size = new System.Drawing.Size(61, 20);
            this.nudPorta.TabIndex = 25;
            // 
            // txtNome
            // 
            this.txtNome.Location = new System.Drawing.Point(9, 32);
            this.txtNome.Name = "txtNome";
            this.txtNome.Size = new System.Drawing.Size(266, 20);
            this.txtNome.TabIndex = 17;
            // 
            // txtEmail
            // 
            this.txtEmail.Location = new System.Drawing.Point(9, 71);
            this.txtEmail.Name = "txtEmail";
            this.txtEmail.Size = new System.Drawing.Size(266, 20);
            this.txtEmail.TabIndex = 18;
            // 
            // label18
            // 
            this.label18.AutoSize = true;
            this.label18.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label18.Location = new System.Drawing.Point(6, 55);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(37, 13);
            this.label18.TabIndex = 24;
            this.label18.Text = "Email";
            // 
            // btnSalvar
            // 
            this.btnSalvar.Location = new System.Drawing.Point(174, 498);
            this.btnSalvar.Name = "btnSalvar";
            this.btnSalvar.Size = new System.Drawing.Size(139, 23);
            this.btnSalvar.TabIndex = 5;
            this.btnSalvar.Text = "Salvar Configurações";
            this.btnSalvar.UseVisualStyleBackColor = true;
            this.btnSalvar.Click += new System.EventHandler(this.BtnSalvar_Click);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.rtbRespostas);
            this.groupBox2.Location = new System.Drawing.Point(319, 231);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(386, 290);
            this.groupBox2.TabIndex = 19;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Respostas";
            // 
            // tabControl3
            // 
            this.tabControl3.Controls.Add(this.tabPage5);
            this.tabControl3.Controls.Add(this.tabPage6);
            this.tabControl3.Controls.Add(this.tabPage7);
            this.tabControl3.Controls.Add(this.tabPage8);
            this.tabControl3.Controls.Add(this.tabPage9);
            this.tabControl3.Location = new System.Drawing.Point(322, 12);
            this.tabControl3.Name = "tabControl3";
            this.tabControl3.SelectedIndex = 0;
            this.tabControl3.Size = new System.Drawing.Size(386, 213);
            this.tabControl3.TabIndex = 24;
            // 
            // tabPage5
            // 
            this.tabPage5.Controls.Add(this.btnClasseAltoNivel);
            this.tabPage5.Controls.Add(this.btnSalvarPDF);
            this.tabPage5.Controls.Add(this.btnGerarChave);
            this.tabPage5.Controls.Add(this.btnLimparLista);
            this.tabPage5.Controls.Add(this.btnImprimirPDF);
            this.tabPage5.Controls.Add(this.btnEnviarAssincrono);
            this.tabPage5.Controls.Add(this.btnEnviarSincrono);
            this.tabPage5.Controls.Add(this.btnCarregarXml);
            this.tabPage5.Controls.Add(this.btnValidarRegra);
            this.tabPage5.Controls.Add(this.btnGerarXml);
            this.tabPage5.Controls.Add(this.btnCarregarIni);
            this.tabPage5.Controls.Add(this.btnAssinar);
            this.tabPage5.Controls.Add(this.btnImprimir);
            this.tabPage5.Controls.Add(this.btnEnviarEmail);
            this.tabPage5.Location = new System.Drawing.Point(4, 22);
            this.tabPage5.Name = "tabPage5";
            this.tabPage5.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage5.Size = new System.Drawing.Size(378, 187);
            this.tabPage5.TabIndex = 0;
            this.tabPage5.Text = "Envio";
            this.tabPage5.UseVisualStyleBackColor = true;
            // 
            // btnOpenSSLInfo
            // 
            this.btnOpenSSLInfo.Location = new System.Drawing.Point(6, 344);
            this.btnOpenSSLInfo.Name = "btnOpenSSLInfo";
            this.btnOpenSSLInfo.Size = new System.Drawing.Size(118, 23);
            this.btnOpenSSLInfo.TabIndex = 21;
            this.btnOpenSSLInfo.Text = "OpenSSLInfo";
            this.btnOpenSSLInfo.UseVisualStyleBackColor = true;
            this.btnOpenSSLInfo.Click += new System.EventHandler(this.btnOpenSSLInfo_Click);
            // 
            // btnSalvarPDF
            // 
            this.btnSalvarPDF.Location = new System.Drawing.Point(130, 93);
            this.btnSalvarPDF.Name = "btnSalvarPDF";
            this.btnSalvarPDF.Size = new System.Drawing.Size(118, 23);
            this.btnSalvarPDF.TabIndex = 27;
            this.btnSalvarPDF.Text = "Salvar PDF (Stream)";
            this.btnSalvarPDF.UseVisualStyleBackColor = true;
            this.btnSalvarPDF.Click += new System.EventHandler(this.btnSalvarPDF_ClickAsync);
            // 
            // btnGerarChave
            // 
            this.btnGerarChave.Location = new System.Drawing.Point(6, 151);
            this.btnGerarChave.Name = "btnGerarChave";
            this.btnGerarChave.Size = new System.Drawing.Size(118, 23);
            this.btnGerarChave.TabIndex = 26;
            this.btnGerarChave.Text = "Gerar Chave NFe ";
            this.btnGerarChave.UseVisualStyleBackColor = true;
            this.btnGerarChave.Click += new System.EventHandler(this.btnGerarChave_Click);
            // 
            // btnLimparLista
            // 
            this.btnLimparLista.Location = new System.Drawing.Point(254, 35);
            this.btnLimparLista.Name = "btnLimparLista";
            this.btnLimparLista.Size = new System.Drawing.Size(118, 23);
            this.btnLimparLista.TabIndex = 25;
            this.btnLimparLista.Text = "Limpar Lista NFe";
            this.btnLimparLista.UseVisualStyleBackColor = true;
            this.btnLimparLista.Click += new System.EventHandler(this.btnLimparLista_Click);
            // 
            // btnImprimirPDF
            // 
            this.btnImprimirPDF.Location = new System.Drawing.Point(130, 65);
            this.btnImprimirPDF.Name = "btnImprimirPDF";
            this.btnImprimirPDF.Size = new System.Drawing.Size(118, 23);
            this.btnImprimirPDF.TabIndex = 24;
            this.btnImprimirPDF.Text = "Imprimir PDF DANFe";
            this.btnImprimirPDF.UseVisualStyleBackColor = true;
            this.btnImprimirPDF.Click += new System.EventHandler(this.btnImprimirPDF_Click);
            // 
            // btnEnviarAssincrono
            // 
            this.btnEnviarAssincrono.Location = new System.Drawing.Point(254, 6);
            this.btnEnviarAssincrono.Name = "btnEnviarAssincrono";
            this.btnEnviarAssincrono.Size = new System.Drawing.Size(118, 23);
            this.btnEnviarAssincrono.TabIndex = 23;
            this.btnEnviarAssincrono.Text = "Enviar Assincrono";
            this.btnEnviarAssincrono.UseVisualStyleBackColor = true;
            this.btnEnviarAssincrono.Click += new System.EventHandler(this.btnEnviarAssincrono_Click);
            // 
            // btnEnviarSincrono
            // 
            this.btnEnviarSincrono.Location = new System.Drawing.Point(130, 6);
            this.btnEnviarSincrono.Name = "btnEnviarSincrono";
            this.btnEnviarSincrono.Size = new System.Drawing.Size(118, 23);
            this.btnEnviarSincrono.TabIndex = 22;
            this.btnEnviarSincrono.Text = "Enviar Sincrono";
            this.btnEnviarSincrono.UseVisualStyleBackColor = true;
            this.btnEnviarSincrono.Click += new System.EventHandler(this.btnEnviarSincrono_Click);
            // 
            // btnCarregarXml
            // 
            this.btnCarregarXml.Location = new System.Drawing.Point(130, 35);
            this.btnCarregarXml.Name = "btnCarregarXml";
            this.btnCarregarXml.Size = new System.Drawing.Size(118, 23);
            this.btnCarregarXml.TabIndex = 21;
            this.btnCarregarXml.Text = "Carregar Xml NFe";
            this.btnCarregarXml.UseVisualStyleBackColor = true;
            this.btnCarregarXml.Click += new System.EventHandler(this.btnCarregarXml_Click);
            // 
            // btnValidarRegra
            // 
            this.btnValidarRegra.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnValidarRegra.Location = new System.Drawing.Point(130, 122);
            this.btnValidarRegra.Name = "btnValidarRegra";
            this.btnValidarRegra.Size = new System.Drawing.Size(118, 23);
            this.btnValidarRegra.TabIndex = 20;
            this.btnValidarRegra.Text = "Val. Regra de Neg.";
            this.btnValidarRegra.UseVisualStyleBackColor = true;
            this.btnValidarRegra.Click += new System.EventHandler(this.btnValidarRegra_Click);
            // 
            // btnGerarXml
            // 
            this.btnGerarXml.Location = new System.Drawing.Point(6, 6);
            this.btnGerarXml.Name = "btnGerarXml";
            this.btnGerarXml.Size = new System.Drawing.Size(118, 23);
            this.btnGerarXml.TabIndex = 10;
            this.btnGerarXml.Text = "Gerar Xml";
            this.btnGerarXml.UseVisualStyleBackColor = true;
            this.btnGerarXml.Click += new System.EventHandler(this.btnGerarXml_Click);
            // 
            // btnCarregarIni
            // 
            this.btnCarregarIni.Location = new System.Drawing.Point(6, 35);
            this.btnCarregarIni.Name = "btnCarregarIni";
            this.btnCarregarIni.Size = new System.Drawing.Size(118, 23);
            this.btnCarregarIni.TabIndex = 18;
            this.btnCarregarIni.Text = "Carregar INI NFe";
            this.btnCarregarIni.UseVisualStyleBackColor = true;
            this.btnCarregarIni.Click += new System.EventHandler(this.btnCarregarIni_Click);
            // 
            // btnAssinar
            // 
            this.btnAssinar.Location = new System.Drawing.Point(6, 122);
            this.btnAssinar.Name = "btnAssinar";
            this.btnAssinar.Size = new System.Drawing.Size(118, 23);
            this.btnAssinar.TabIndex = 19;
            this.btnAssinar.Text = "Assinar NFe ";
            this.btnAssinar.UseVisualStyleBackColor = true;
            this.btnAssinar.Click += new System.EventHandler(this.btnAssinar_Click);
            // 
            // btnImprimir
            // 
            this.btnImprimir.Location = new System.Drawing.Point(6, 64);
            this.btnImprimir.Name = "btnImprimir";
            this.btnImprimir.Size = new System.Drawing.Size(118, 23);
            this.btnImprimir.TabIndex = 11;
            this.btnImprimir.Text = "Imprimir DANFe";
            this.btnImprimir.UseVisualStyleBackColor = true;
            this.btnImprimir.Click += new System.EventHandler(this.btnImprimir_Click);
            // 
            // btnEnviarEmail
            // 
            this.btnEnviarEmail.Location = new System.Drawing.Point(6, 93);
            this.btnEnviarEmail.Name = "btnEnviarEmail";
            this.btnEnviarEmail.Size = new System.Drawing.Size(118, 23);
            this.btnEnviarEmail.TabIndex = 14;
            this.btnEnviarEmail.Text = "Enviar NFe Email";
            this.btnEnviarEmail.UseVisualStyleBackColor = true;
            this.btnEnviarEmail.Click += new System.EventHandler(this.btnEnviarEmail_Click);
            // 
            // tabPage6
            // 
            this.tabPage6.Controls.Add(this.btnConsultarCadastro);
            this.tabPage6.Controls.Add(this.btnStatusServ);
            this.tabPage6.Controls.Add(this.btnConsultaXml);
            this.tabPage6.Controls.Add(this.btnConsultaChave);
            this.tabPage6.Controls.Add(this.btnConsultarRecibo);
            this.tabPage6.Location = new System.Drawing.Point(4, 22);
            this.tabPage6.Name = "tabPage6";
            this.tabPage6.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage6.Size = new System.Drawing.Size(378, 187);
            this.tabPage6.TabIndex = 1;
            this.tabPage6.Text = "Consultas";
            this.tabPage6.UseVisualStyleBackColor = true;
            // 
            // btnConsultarCadastro
            // 
            this.btnConsultarCadastro.Location = new System.Drawing.Point(130, 35);
            this.btnConsultarCadastro.Name = "btnConsultarCadastro";
            this.btnConsultarCadastro.Size = new System.Drawing.Size(118, 23);
            this.btnConsultarCadastro.TabIndex = 16;
            this.btnConsultarCadastro.Text = "Consultar Cadastro";
            this.btnConsultarCadastro.UseVisualStyleBackColor = true;
            this.btnConsultarCadastro.Click += new System.EventHandler(this.btnConsultarCadastro_Click);
            // 
            // btnStatusServ
            // 
            this.btnStatusServ.Location = new System.Drawing.Point(6, 6);
            this.btnStatusServ.Name = "btnStatusServ";
            this.btnStatusServ.Size = new System.Drawing.Size(118, 23);
            this.btnStatusServ.TabIndex = 9;
            this.btnStatusServ.Text = " Status de Serviço";
            this.btnStatusServ.UseVisualStyleBackColor = true;
            this.btnStatusServ.Click += new System.EventHandler(this.btnStatusServ_Click);
            // 
            // btnConsultaXml
            // 
            this.btnConsultaXml.Location = new System.Drawing.Point(130, 6);
            this.btnConsultaXml.Name = "btnConsultaXml";
            this.btnConsultaXml.Size = new System.Drawing.Size(118, 23);
            this.btnConsultaXml.TabIndex = 12;
            this.btnConsultaXml.Text = "Consultar com Xml";
            this.btnConsultaXml.UseVisualStyleBackColor = true;
            this.btnConsultaXml.Click += new System.EventHandler(this.btnConsultaXml_Click);
            // 
            // btnConsultaChave
            // 
            this.btnConsultaChave.Location = new System.Drawing.Point(254, 6);
            this.btnConsultaChave.Name = "btnConsultaChave";
            this.btnConsultaChave.Size = new System.Drawing.Size(118, 23);
            this.btnConsultaChave.TabIndex = 13;
            this.btnConsultaChave.Text = "Consultar com Chave";
            this.btnConsultaChave.UseVisualStyleBackColor = true;
            this.btnConsultaChave.Click += new System.EventHandler(this.btnConsultaChave_Click);
            // 
            // btnConsultarRecibo
            // 
            this.btnConsultarRecibo.Location = new System.Drawing.Point(6, 35);
            this.btnConsultarRecibo.Name = "btnConsultarRecibo";
            this.btnConsultarRecibo.Size = new System.Drawing.Size(118, 23);
            this.btnConsultarRecibo.TabIndex = 15;
            this.btnConsultarRecibo.Text = "Consultar Recibo";
            this.btnConsultarRecibo.UseVisualStyleBackColor = true;
            this.btnConsultarRecibo.Click += new System.EventHandler(this.btnConsultarRecibo_Click);
            // 
            // tabPage7
            // 
            this.tabPage7.Controls.Add(this.btnEnviarEvento);
            this.tabPage7.Controls.Add(this.btnImprimirEventoPDF);
            this.tabPage7.Controls.Add(this.btnLimparListaEvento);
            this.tabPage7.Controls.Add(this.btnImprimirEvento);
            this.tabPage7.Controls.Add(this.btnEnviarEmailEvento);
            this.tabPage7.Controls.Add(this.btnCarregarEvento);
            this.tabPage7.Controls.Add(this.btnCancelar);
            this.tabPage7.Location = new System.Drawing.Point(4, 22);
            this.tabPage7.Name = "tabPage7";
            this.tabPage7.Size = new System.Drawing.Size(378, 187);
            this.tabPage7.TabIndex = 2;
            this.tabPage7.Text = "Eventos";
            this.tabPage7.UseVisualStyleBackColor = true;
            // 
            // btnEnviarEvento
            // 
            this.btnEnviarEvento.Location = new System.Drawing.Point(130, 6);
            this.btnEnviarEvento.Name = "btnEnviarEvento";
            this.btnEnviarEvento.Size = new System.Drawing.Size(118, 23);
            this.btnEnviarEvento.TabIndex = 31;
            this.btnEnviarEvento.Text = "Enviar Evento";
            this.btnEnviarEvento.UseVisualStyleBackColor = true;
            this.btnEnviarEvento.Click += new System.EventHandler(this.btnEnviarEvento_Click);
            // 
            // btnImprimirEventoPDF
            // 
            this.btnImprimirEventoPDF.Location = new System.Drawing.Point(130, 65);
            this.btnImprimirEventoPDF.Name = "btnImprimirEventoPDF";
            this.btnImprimirEventoPDF.Size = new System.Drawing.Size(118, 23);
            this.btnImprimirEventoPDF.TabIndex = 30;
            this.btnImprimirEventoPDF.Text = "Imprimir PDF Evento";
            this.btnImprimirEventoPDF.UseVisualStyleBackColor = true;
            this.btnImprimirEventoPDF.Click += new System.EventHandler(this.btnImprimirEventoPDF_Click);
            // 
            // btnLimparListaEvento
            // 
            this.btnLimparListaEvento.Location = new System.Drawing.Point(130, 35);
            this.btnLimparListaEvento.Name = "btnLimparListaEvento";
            this.btnLimparListaEvento.Size = new System.Drawing.Size(118, 23);
            this.btnLimparListaEvento.TabIndex = 29;
            this.btnLimparListaEvento.Text = "Limpar LIsta Eventos";
            this.btnLimparListaEvento.UseVisualStyleBackColor = true;
            this.btnLimparListaEvento.Click += new System.EventHandler(this.btnLimparListaEvento_Click);
            // 
            // btnImprimirEvento
            // 
            this.btnImprimirEvento.Location = new System.Drawing.Point(6, 64);
            this.btnImprimirEvento.Name = "btnImprimirEvento";
            this.btnImprimirEvento.Size = new System.Drawing.Size(118, 23);
            this.btnImprimirEvento.TabIndex = 28;
            this.btnImprimirEvento.Text = "Imprimir Evento";
            this.btnImprimirEvento.UseVisualStyleBackColor = true;
            this.btnImprimirEvento.Click += new System.EventHandler(this.btnImprimirEvento_Click);
            // 
            // btnEnviarEmailEvento
            // 
            this.btnEnviarEmailEvento.Location = new System.Drawing.Point(6, 93);
            this.btnEnviarEmailEvento.Name = "btnEnviarEmailEvento";
            this.btnEnviarEmailEvento.Size = new System.Drawing.Size(118, 23);
            this.btnEnviarEmailEvento.TabIndex = 27;
            this.btnEnviarEmailEvento.Text = "Enviar Evento Email";
            this.btnEnviarEmailEvento.UseVisualStyleBackColor = true;
            this.btnEnviarEmailEvento.Click += new System.EventHandler(this.btnEnviarEmailEvento_Click);
            // 
            // btnCarregarEvento
            // 
            this.btnCarregarEvento.Location = new System.Drawing.Point(6, 35);
            this.btnCarregarEvento.Name = "btnCarregarEvento";
            this.btnCarregarEvento.Size = new System.Drawing.Size(118, 23);
            this.btnCarregarEvento.TabIndex = 17;
            this.btnCarregarEvento.Text = "Carregar Evento";
            this.btnCarregarEvento.UseVisualStyleBackColor = true;
            this.btnCarregarEvento.Click += new System.EventHandler(this.btnCarregarEvento_Click);
            // 
            // btnCancelar
            // 
            this.btnCancelar.Location = new System.Drawing.Point(6, 6);
            this.btnCancelar.Name = "btnCancelar";
            this.btnCancelar.Size = new System.Drawing.Size(118, 23);
            this.btnCancelar.TabIndex = 16;
            this.btnCancelar.Text = "Cancelar NFe";
            this.btnCancelar.UseVisualStyleBackColor = true;
            this.btnCancelar.Click += new System.EventHandler(this.btnCancelar_Click);
            // 
            // tabPage8
            // 
            this.tabPage8.Controls.Add(this.btnImprimirInutilizacaoPDF);
            this.tabPage8.Controls.Add(this.btnImprimirInutilizacao);
            this.tabPage8.Controls.Add(this.btnInutilizar);
            this.tabPage8.Location = new System.Drawing.Point(4, 22);
            this.tabPage8.Name = "tabPage8";
            this.tabPage8.Size = new System.Drawing.Size(378, 187);
            this.tabPage8.TabIndex = 3;
            this.tabPage8.Text = "Inutilização";
            this.tabPage8.UseVisualStyleBackColor = true;
            // 
            // btnImprimirInutilizacaoPDF
            // 
            this.btnImprimirInutilizacaoPDF.Location = new System.Drawing.Point(130, 35);
            this.btnImprimirInutilizacaoPDF.Name = "btnImprimirInutilizacaoPDF";
            this.btnImprimirInutilizacaoPDF.Size = new System.Drawing.Size(133, 23);
            this.btnImprimirInutilizacaoPDF.TabIndex = 32;
            this.btnImprimirInutilizacaoPDF.Text = "Imprimir PDF Inutilização";
            this.btnImprimirInutilizacaoPDF.UseVisualStyleBackColor = true;
            this.btnImprimirInutilizacaoPDF.Click += new System.EventHandler(this.btnImprimirInutilizacaoPDF_Click);
            // 
            // btnImprimirInutilizacao
            // 
            this.btnImprimirInutilizacao.Location = new System.Drawing.Point(6, 35);
            this.btnImprimirInutilizacao.Name = "btnImprimirInutilizacao";
            this.btnImprimirInutilizacao.Size = new System.Drawing.Size(118, 23);
            this.btnImprimirInutilizacao.TabIndex = 31;
            this.btnImprimirInutilizacao.Text = "Imprimir Inutilização";
            this.btnImprimirInutilizacao.UseVisualStyleBackColor = true;
            this.btnImprimirInutilizacao.Click += new System.EventHandler(this.btnImprimirInutilizacao_Click);
            // 
            // btnInutilizar
            // 
            this.btnInutilizar.Location = new System.Drawing.Point(6, 6);
            this.btnInutilizar.Name = "btnInutilizar";
            this.btnInutilizar.Size = new System.Drawing.Size(118, 23);
            this.btnInutilizar.TabIndex = 17;
            this.btnInutilizar.Text = "Inutilizar Numeração";
            this.btnInutilizar.UseVisualStyleBackColor = true;
            this.btnInutilizar.Click += new System.EventHandler(this.btnInutilizar_Click);
            // 
            // tabPage9
            // 
            this.tabPage9.Controls.Add(this.btnDFePorUltNSU);
            this.tabPage9.Controls.Add(this.btnDFePorNSU);
            this.tabPage9.Controls.Add(this.btnDFePorChave);
            this.tabPage9.Location = new System.Drawing.Point(4, 22);
            this.tabPage9.Name = "tabPage9";
            this.tabPage9.Size = new System.Drawing.Size(378, 187);
            this.tabPage9.TabIndex = 4;
            this.tabPage9.Text = "Distribuição DFe";
            this.tabPage9.UseVisualStyleBackColor = true;
            // 
            // btnDFePorUltNSU
            // 
            this.btnDFePorUltNSU.Location = new System.Drawing.Point(254, 6);
            this.btnDFePorUltNSU.Name = "btnDFePorUltNSU";
            this.btnDFePorUltNSU.Size = new System.Drawing.Size(118, 23);
            this.btnDFePorUltNSU.TabIndex = 21;
            this.btnDFePorUltNSU.Text = "Por Ult. NSU";
            this.btnDFePorUltNSU.UseVisualStyleBackColor = true;
            this.btnDFePorUltNSU.Click += new System.EventHandler(this.btnDFePorUltNSU_Click);
            // 
            // btnDFePorNSU
            // 
            this.btnDFePorNSU.Location = new System.Drawing.Point(130, 6);
            this.btnDFePorNSU.Name = "btnDFePorNSU";
            this.btnDFePorNSU.Size = new System.Drawing.Size(118, 23);
            this.btnDFePorNSU.TabIndex = 20;
            this.btnDFePorNSU.Text = "Por NSU";
            this.btnDFePorNSU.UseVisualStyleBackColor = true;
            this.btnDFePorNSU.Click += new System.EventHandler(this.btnDFePorNSU_Click);
            // 
            // btnDFePorChave
            // 
            this.btnDFePorChave.Location = new System.Drawing.Point(6, 6);
            this.btnDFePorChave.Name = "btnDFePorChave";
            this.btnDFePorChave.Size = new System.Drawing.Size(118, 23);
            this.btnDFePorChave.TabIndex = 19;
            this.btnDFePorChave.Text = "Por Chave";
            this.btnDFePorChave.UseVisualStyleBackColor = true;
            this.btnDFePorChave.Click += new System.EventHandler(this.btnDFePorChave_Click);
            // 
            // btnCarregarConfiguracoes
            // 
            this.btnCarregarConfiguracoes.Location = new System.Drawing.Point(12, 498);
            this.btnCarregarConfiguracoes.Name = "btnCarregarConfiguracoes";
            this.btnCarregarConfiguracoes.Size = new System.Drawing.Size(139, 23);
            this.btnCarregarConfiguracoes.TabIndex = 25;
            this.btnCarregarConfiguracoes.Text = "Carregar Configurações";
            this.btnCarregarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnCarregarConfiguracoes.Click += new System.EventHandler(this.btnCarregarConfiguracoes_Click);
            // 
            // btnClasseAltoNivel
            // 
            this.btnClasseAltoNivel.Location = new System.Drawing.Point(254, 154);
            this.btnClasseAltoNivel.Name = "btnClasseAltoNivel";
            this.btnClasseAltoNivel.Size = new System.Drawing.Size(118, 23);
            this.btnClasseAltoNivel.TabIndex = 28;
            this.btnClasseAltoNivel.Text = "Classe Alto Nivel";
            this.btnClasseAltoNivel.UseVisualStyleBackColor = true;
            this.btnClasseAltoNivel.Click += new System.EventHandler(this.btnClasseAltoNivel_Click);
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(716, 533);
            this.Controls.Add(this.btnCarregarConfiguracoes);
            this.Controls.Add(this.tabControl3);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.btnSalvar);
            this.Controls.Add(this.tabControl1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibNFe Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.tabControl1.ResumeLayout(false);
            this.tbpConfiguracoes.ResumeLayout(false);
            this.tabControl2.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudIntervalos)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudTentativas)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudAguardar)).EndInit();
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
            this.tbpDocumentoAuxiliar.ResumeLayout(false);
            this.tbpDocumentoAuxiliar.PerformLayout();
            this.groupBox9.ResumeLayout(false);
            this.groupBox9.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudLinhasPular)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudBuffer)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudEspacos)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudColunas)).EndInit();
            this.groupBox8.ResumeLayout(false);
            this.groupBox8.PerformLayout();
            this.groupBox7.ResumeLayout(false);
            this.groupBox7.PerformLayout();
            this.tbpEmail.ResumeLayout(false);
            this.tbpEmail.PerformLayout();
            this.groupBox6.ResumeLayout(false);
            this.groupBox6.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudPorta)).EndInit();
            this.groupBox2.ResumeLayout(false);
            this.tabControl3.ResumeLayout(false);
            this.tabPage5.ResumeLayout(false);
            this.tabPage6.ResumeLayout(false);
            this.tabPage7.ResumeLayout(false);
            this.tabPage8.ResumeLayout(false);
            this.tabPage9.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.RichTextBox rtbRespostas;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tbpDocumentoAuxiliar;
        private System.Windows.Forms.Button btnSalvar;
        private System.Windows.Forms.TabPage tbpConfiguracoes;
        private System.Windows.Forms.TextBox txtCSC;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox txtIdCSC;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox txtLogs;
        private System.Windows.Forms.Button btnSelectLog;
        private System.Windows.Forms.ComboBox cmbModeloDocumento;
        private System.Windows.Forms.Label label17;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TabPage tbpEmail;
        private System.Windows.Forms.TextBox txtSenha;
        private System.Windows.Forms.Label label22;
        private System.Windows.Forms.TextBox txtUsuario;
        private System.Windows.Forms.Label label23;
        private System.Windows.Forms.Label label21;
        private System.Windows.Forms.NumericUpDown nudPorta;
        private System.Windows.Forms.TextBox txtEmail;
        private System.Windows.Forms.Label label18;
        private System.Windows.Forms.TextBox txtNome;
        private System.Windows.Forms.Label label19;
        private System.Windows.Forms.CheckBox ckbTLS;
        private System.Windows.Forms.CheckBox ckbSSL;
        private System.Windows.Forms.TextBox txtHost;
        private System.Windows.Forms.Label label20;
        private System.Windows.Forms.GroupBox groupBox6;
        private System.Windows.Forms.TextBox txtAssunto;
        private System.Windows.Forms.Label label24;
        private System.Windows.Forms.TextBox txtMensagem;
        private System.Windows.Forms.Label label25;
        private System.Windows.Forms.TabControl tabControl2;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.TabPage tabPage4;
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
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckBox ckbSalvarSOAP;
        private System.Windows.Forms.CheckBox ckbVisualizar;
        private System.Windows.Forms.NumericUpDown nudIntervalos;
        private System.Windows.Forms.Label label29;
        private System.Windows.Forms.NumericUpDown nudTentativas;
        private System.Windows.Forms.Label label28;
        private System.Windows.Forms.NumericUpDown nudAguardar;
        private System.Windows.Forms.Label label27;
        private System.Windows.Forms.CheckBox ckbAjustarAut;
        private System.Windows.Forms.Label label31;
        private System.Windows.Forms.ComboBox cmbFormaEmissao;
        private System.Windows.Forms.Label label30;
        private System.Windows.Forms.TextBox txtFormatoAlerta;
        private System.Windows.Forms.CheckBox ckbExibirErroSchema;
        private System.Windows.Forms.CheckBox ckbAtualizarXML;
        private System.Windows.Forms.Label label32;
        private System.Windows.Forms.ComboBox cmbVersaoDF;
        private System.Windows.Forms.TextBox txtSchemaPath;
        private System.Windows.Forms.Label label33;
        private System.Windows.Forms.Button btnSelectSchema;
        private System.Windows.Forms.CheckBox ckbSalvar;
        private System.Windows.Forms.CheckBox ckbRetirarAcentos;
        private System.Windows.Forms.TextBox txtArqEvento;
        private System.Windows.Forms.Button btnArqEvento;
        private System.Windows.Forms.Label label34;
        private System.Windows.Forms.TextBox txtArqInu;
        private System.Windows.Forms.Button btnArqInu;
        private System.Windows.Forms.Label label35;
        private System.Windows.Forms.TextBox txtArqNFe;
        private System.Windows.Forms.Button btnArqNFe;
        private System.Windows.Forms.Label label36;
        private System.Windows.Forms.CheckBox ckbSepararPorModelo;
        private System.Windows.Forms.CheckBox ckbSepararPorCNPJ;
        private System.Windows.Forms.CheckBox ckbSalvaPathEvento;
        private System.Windows.Forms.CheckBox ckbEmissaoPathNFe;
        private System.Windows.Forms.CheckBox ckbAdicionaLiteral;
        private System.Windows.Forms.CheckBox ckbPastaMensal;
        private System.Windows.Forms.CheckBox ckbSalvarArqs;
        private System.Windows.Forms.GroupBox groupBox7;
        private System.Windows.Forms.RadioButton rdbPaisagem;
        private System.Windows.Forms.RadioButton rdbRetrato;
        private System.Windows.Forms.TextBox txtLogomarca;
        private System.Windows.Forms.Button btnLogomarca;
        private System.Windows.Forms.Label label37;
        private System.Windows.Forms.GroupBox groupBox9;
        private System.Windows.Forms.ComboBox cbbPaginaCodigo;
        private System.Windows.Forms.Label label38;
        private System.Windows.Forms.CheckBox cbxIgnorarTags;
        private System.Windows.Forms.CheckBox cbxTraduzirTags;
        private System.Windows.Forms.CheckBox cbxCortarPapel;
        private System.Windows.Forms.CheckBox cbxControlePorta;
        private System.Windows.Forms.Label label39;
        private System.Windows.Forms.NumericUpDown nudLinhasPular;
        private System.Windows.Forms.Label label40;
        private System.Windows.Forms.NumericUpDown nudBuffer;
        private System.Windows.Forms.Label label41;
        private System.Windows.Forms.NumericUpDown nudEspacos;
        private System.Windows.Forms.Label label42;
        private System.Windows.Forms.Label label43;
        private System.Windows.Forms.NumericUpDown nudColunas;
        private System.Windows.Forms.ComboBox cbbPortas;
        private System.Windows.Forms.ComboBox cbbModelo;
        private System.Windows.Forms.Label label44;
        private System.Windows.Forms.GroupBox groupBox8;
        private System.Windows.Forms.RadioButton rdbFortesA4;
        private System.Windows.Forms.RadioButton rdbEscPos;
        private System.Windows.Forms.RadioButton rdbFortes;
        private System.Windows.Forms.TabControl tabControl3;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.Button btnLimparLista;
        private System.Windows.Forms.Button btnImprimirPDF;
        private System.Windows.Forms.Button btnEnviarAssincrono;
        private System.Windows.Forms.Button btnEnviarSincrono;
        private System.Windows.Forms.Button btnCarregarXml;
        private System.Windows.Forms.Button btnValidarRegra;
        private System.Windows.Forms.Button btnGerarXml;
        private System.Windows.Forms.Button btnCarregarIni;
        private System.Windows.Forms.Button btnAssinar;
        private System.Windows.Forms.Button btnImprimir;
        private System.Windows.Forms.Button btnEnviarEmail;
        private System.Windows.Forms.TabPage tabPage6;
        private System.Windows.Forms.Button btnConsultarCadastro;
        private System.Windows.Forms.Button btnStatusServ;
        private System.Windows.Forms.Button btnConsultaXml;
        private System.Windows.Forms.Button btnConsultaChave;
        private System.Windows.Forms.Button btnConsultarRecibo;
        private System.Windows.Forms.TabPage tabPage7;
        private System.Windows.Forms.Button btnEnviarEvento;
        private System.Windows.Forms.Button btnImprimirEventoPDF;
        private System.Windows.Forms.Button btnLimparListaEvento;
        private System.Windows.Forms.Button btnImprimirEvento;
        private System.Windows.Forms.Button btnEnviarEmailEvento;
        private System.Windows.Forms.Button btnCarregarEvento;
        private System.Windows.Forms.Button btnCancelar;
        private System.Windows.Forms.TabPage tabPage8;
        private System.Windows.Forms.Button btnImprimirInutilizacaoPDF;
        private System.Windows.Forms.Button btnImprimirInutilizacao;
        private System.Windows.Forms.Button btnInutilizar;
        private System.Windows.Forms.TabPage tabPage9;
        private System.Windows.Forms.Button btnDFePorUltNSU;
        private System.Windows.Forms.Button btnDFePorNSU;
        private System.Windows.Forms.Button btnDFePorChave;
        private System.Windows.Forms.Button btnObterCertificados;
        private System.Windows.Forms.Button btnCarregarConfiguracoes;
        private System.Windows.Forms.Button btnGerarChave;
        private System.Windows.Forms.Button btnSalvarPDF;
        private System.Windows.Forms.Button btnClasseAltoNivel;
        private System.Windows.Forms.Button btnOpenSSLInfo;
    }
}

