namespace ACBrLibCEP.Demo
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
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tPageConfiguracao = new System.Windows.Forms.TabPage();
            this.grpBoxProxy = new System.Windows.Forms.GroupBox();
            this.nudProxyPorta = new System.Windows.Forms.NumericUpDown();
            this.txtSenhaProxy = new System.Windows.Forms.TextBox();
            this.lblSenhaProxy = new System.Windows.Forms.Label();
            this.txtUsuarioProxy = new System.Windows.Forms.TextBox();
            this.lblUsuarioProxy = new System.Windows.Forms.Label();
            this.lblPortaProxy = new System.Windows.Forms.Label();
            this.txtHostProxy = new System.Windows.Forms.TextBox();
            this.lblHostProxy = new System.Windows.Forms.Label();
            this.grpBoxWebService = new System.Windows.Forms.GroupBox();
            this.cmbWebService = new System.Windows.Forms.ComboBox();
            this.txtChaveWebService = new System.Windows.Forms.TextBox();
            this.lblChaveWebService = new System.Windows.Forms.Label();
            this.txtSenhaWebService = new System.Windows.Forms.TextBox();
            this.lblSenhaWebService = new System.Windows.Forms.Label();
            this.txtUsuarioWebService = new System.Windows.Forms.TextBox();
            this.lblUsuarioWebService = new System.Windows.Forms.Label();
            this.btnCarregarConfiguracoes = new System.Windows.Forms.Button();
            this.btnSalvarConfiguracoes = new System.Windows.Forms.Button();
            this.tPageBuscarCEP = new System.Windows.Forms.TabPage();
            this.grpBoxBuscarPorLogradouro = new System.Windows.Forms.GroupBox();
            this.txtBairro = new System.Windows.Forms.TextBox();
            this.lblBairro = new System.Windows.Forms.Label();
            this.txtUF = new System.Windows.Forms.TextBox();
            this.lblUF = new System.Windows.Forms.Label();
            this.txtCidade = new System.Windows.Forms.TextBox();
            this.lblCidade = new System.Windows.Forms.Label();
            this.txtTipoLogradouro = new System.Windows.Forms.TextBox();
            this.lblTipoEndereco = new System.Windows.Forms.Label();
            this.btnBuscarPorLogradouro = new System.Windows.Forms.Button();
            this.txtLogradouro = new System.Windows.Forms.TextBox();
            this.lblLogradouro = new System.Windows.Forms.Label();
            this.grpBoxBuscarPorCEP = new System.Windows.Forms.GroupBox();
            this.btnBuscarPorCEP = new System.Windows.Forms.Button();
            this.txtCEP = new System.Windows.Forms.TextBox();
            this.lblCEP = new System.Windows.Forms.Label();
            this.txtRetorno = new System.Windows.Forms.TextBox();
            this.chkPesquisarIBGE = new System.Windows.Forms.CheckBox();
            this.tabControl1.SuspendLayout();
            this.tPageConfiguracao.SuspendLayout();
            this.grpBoxProxy.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudProxyPorta)).BeginInit();
            this.grpBoxWebService.SuspendLayout();
            this.tPageBuscarCEP.SuspendLayout();
            this.grpBoxBuscarPorLogradouro.SuspendLayout();
            this.grpBoxBuscarPorCEP.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tPageConfiguracao);
            this.tabControl1.Controls.Add(this.tPageBuscarCEP);
            this.tabControl1.Location = new System.Drawing.Point(12, 12);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(603, 254);
            this.tabControl1.TabIndex = 18;
            // 
            // tPageConfiguracao
            // 
            this.tPageConfiguracao.Controls.Add(this.grpBoxProxy);
            this.tPageConfiguracao.Controls.Add(this.grpBoxWebService);
            this.tPageConfiguracao.Controls.Add(this.btnCarregarConfiguracoes);
            this.tPageConfiguracao.Controls.Add(this.btnSalvarConfiguracoes);
            this.tPageConfiguracao.Location = new System.Drawing.Point(4, 22);
            this.tPageConfiguracao.Name = "tPageConfiguracao";
            this.tPageConfiguracao.Padding = new System.Windows.Forms.Padding(3);
            this.tPageConfiguracao.Size = new System.Drawing.Size(595, 228);
            this.tPageConfiguracao.TabIndex = 0;
            this.tPageConfiguracao.Text = "Configuração";
            this.tPageConfiguracao.UseVisualStyleBackColor = true;
            // 
            // grpBoxProxy
            // 
            this.grpBoxProxy.Controls.Add(this.nudProxyPorta);
            this.grpBoxProxy.Controls.Add(this.txtSenhaProxy);
            this.grpBoxProxy.Controls.Add(this.lblSenhaProxy);
            this.grpBoxProxy.Controls.Add(this.txtUsuarioProxy);
            this.grpBoxProxy.Controls.Add(this.lblUsuarioProxy);
            this.grpBoxProxy.Controls.Add(this.lblPortaProxy);
            this.grpBoxProxy.Controls.Add(this.txtHostProxy);
            this.grpBoxProxy.Controls.Add(this.lblHostProxy);
            this.grpBoxProxy.Location = new System.Drawing.Point(279, 6);
            this.grpBoxProxy.Name = "grpBoxProxy";
            this.grpBoxProxy.Size = new System.Drawing.Size(307, 176);
            this.grpBoxProxy.TabIndex = 17;
            this.grpBoxProxy.TabStop = false;
            this.grpBoxProxy.Text = "Proxy";
            // 
            // nudProxyPorta
            // 
            this.nudProxyPorta.Location = new System.Drawing.Point(184, 38);
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
            this.nudProxyPorta.Size = new System.Drawing.Size(100, 20);
            this.nudProxyPorta.TabIndex = 19;
            this.nudProxyPorta.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.nudProxyPorta.Value = new decimal(new int[] {
            5000,
            0,
            0,
            0});
            // 
            // txtSenhaProxy
            // 
            this.txtSenhaProxy.Location = new System.Drawing.Point(149, 85);
            this.txtSenhaProxy.Name = "txtSenhaProxy";
            this.txtSenhaProxy.Size = new System.Drawing.Size(135, 20);
            this.txtSenhaProxy.TabIndex = 9;
            // 
            // lblSenhaProxy
            // 
            this.lblSenhaProxy.AutoSize = true;
            this.lblSenhaProxy.Location = new System.Drawing.Point(146, 69);
            this.lblSenhaProxy.Name = "lblSenhaProxy";
            this.lblSenhaProxy.Size = new System.Drawing.Size(38, 13);
            this.lblSenhaProxy.TabIndex = 8;
            this.lblSenhaProxy.Text = "Senha";
            // 
            // txtUsuarioProxy
            // 
            this.txtUsuarioProxy.Location = new System.Drawing.Point(19, 85);
            this.txtUsuarioProxy.Name = "txtUsuarioProxy";
            this.txtUsuarioProxy.Size = new System.Drawing.Size(124, 20);
            this.txtUsuarioProxy.TabIndex = 7;
            // 
            // lblUsuarioProxy
            // 
            this.lblUsuarioProxy.AutoSize = true;
            this.lblUsuarioProxy.Location = new System.Drawing.Point(16, 69);
            this.lblUsuarioProxy.Name = "lblUsuarioProxy";
            this.lblUsuarioProxy.Size = new System.Drawing.Size(43, 13);
            this.lblUsuarioProxy.TabIndex = 6;
            this.lblUsuarioProxy.Text = "Usuário";
            // 
            // lblPortaProxy
            // 
            this.lblPortaProxy.AutoSize = true;
            this.lblPortaProxy.Location = new System.Drawing.Point(181, 22);
            this.lblPortaProxy.Name = "lblPortaProxy";
            this.lblPortaProxy.Size = new System.Drawing.Size(32, 13);
            this.lblPortaProxy.TabIndex = 4;
            this.lblPortaProxy.Text = "Porta";
            // 
            // txtHostProxy
            // 
            this.txtHostProxy.Location = new System.Drawing.Point(19, 38);
            this.txtHostProxy.Name = "txtHostProxy";
            this.txtHostProxy.Size = new System.Drawing.Size(159, 20);
            this.txtHostProxy.TabIndex = 3;
            // 
            // lblHostProxy
            // 
            this.lblHostProxy.AutoSize = true;
            this.lblHostProxy.Location = new System.Drawing.Point(16, 22);
            this.lblHostProxy.Name = "lblHostProxy";
            this.lblHostProxy.Size = new System.Drawing.Size(29, 13);
            this.lblHostProxy.TabIndex = 2;
            this.lblHostProxy.Text = "Host";
            // 
            // grpBoxWebService
            // 
            this.grpBoxWebService.Controls.Add(this.chkPesquisarIBGE);
            this.grpBoxWebService.Controls.Add(this.cmbWebService);
            this.grpBoxWebService.Controls.Add(this.txtChaveWebService);
            this.grpBoxWebService.Controls.Add(this.lblChaveWebService);
            this.grpBoxWebService.Controls.Add(this.txtSenhaWebService);
            this.grpBoxWebService.Controls.Add(this.lblSenhaWebService);
            this.grpBoxWebService.Controls.Add(this.txtUsuarioWebService);
            this.grpBoxWebService.Controls.Add(this.lblUsuarioWebService);
            this.grpBoxWebService.Location = new System.Drawing.Point(6, 6);
            this.grpBoxWebService.Name = "grpBoxWebService";
            this.grpBoxWebService.Size = new System.Drawing.Size(267, 176);
            this.grpBoxWebService.TabIndex = 16;
            this.grpBoxWebService.TabStop = false;
            this.grpBoxWebService.Text = "WebService";
            // 
            // cmbWebService
            // 
            this.cmbWebService.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbWebService.FormattingEnabled = true;
            this.cmbWebService.Items.AddRange(new object[] {
            "wsNenhum",
            "wsBuscarCep",
            "wsCepLivre",
            "wsRepublicaVirtual",
            "wsBases4you",
            "wsRNSolucoes",
            "wsKingHost",
            "wsByJG",
            "wsCorreios",
            "wsDevMedia",
            "wsViaCep",
            "wsCorreiosSIGEP",
            "wsCepAberto",
            "wsWSCep"});
            this.cmbWebService.Location = new System.Drawing.Point(6, 19);
            this.cmbWebService.Name = "cmbWebService";
            this.cmbWebService.Size = new System.Drawing.Size(134, 21);
            this.cmbWebService.TabIndex = 11;
            // 
            // txtChaveWebService
            // 
            this.txtChaveWebService.Location = new System.Drawing.Point(161, 133);
            this.txtChaveWebService.Name = "txtChaveWebService";
            this.txtChaveWebService.Size = new System.Drawing.Size(100, 20);
            this.txtChaveWebService.TabIndex = 6;
            // 
            // lblChaveWebService
            // 
            this.lblChaveWebService.AutoSize = true;
            this.lblChaveWebService.Location = new System.Drawing.Point(158, 117);
            this.lblChaveWebService.Name = "lblChaveWebService";
            this.lblChaveWebService.Size = new System.Drawing.Size(38, 13);
            this.lblChaveWebService.TabIndex = 5;
            this.lblChaveWebService.Text = "Chave";
            // 
            // txtSenhaWebService
            // 
            this.txtSenhaWebService.Location = new System.Drawing.Point(161, 85);
            this.txtSenhaWebService.Name = "txtSenhaWebService";
            this.txtSenhaWebService.Size = new System.Drawing.Size(100, 20);
            this.txtSenhaWebService.TabIndex = 4;
            // 
            // lblSenhaWebService
            // 
            this.lblSenhaWebService.AutoSize = true;
            this.lblSenhaWebService.Location = new System.Drawing.Point(158, 69);
            this.lblSenhaWebService.Name = "lblSenhaWebService";
            this.lblSenhaWebService.Size = new System.Drawing.Size(38, 13);
            this.lblSenhaWebService.TabIndex = 3;
            this.lblSenhaWebService.Text = "Senha";
            // 
            // txtUsuarioWebService
            // 
            this.txtUsuarioWebService.Location = new System.Drawing.Point(161, 38);
            this.txtUsuarioWebService.Name = "txtUsuarioWebService";
            this.txtUsuarioWebService.Size = new System.Drawing.Size(100, 20);
            this.txtUsuarioWebService.TabIndex = 2;
            // 
            // lblUsuarioWebService
            // 
            this.lblUsuarioWebService.AutoSize = true;
            this.lblUsuarioWebService.Location = new System.Drawing.Point(158, 22);
            this.lblUsuarioWebService.Name = "lblUsuarioWebService";
            this.lblUsuarioWebService.Size = new System.Drawing.Size(43, 13);
            this.lblUsuarioWebService.TabIndex = 1;
            this.lblUsuarioWebService.Text = "Usuário";
            // 
            // btnCarregarConfiguracoes
            // 
            this.btnCarregarConfiguracoes.Location = new System.Drawing.Point(178, 188);
            this.btnCarregarConfiguracoes.Name = "btnCarregarConfiguracoes";
            this.btnCarregarConfiguracoes.Size = new System.Drawing.Size(166, 34);
            this.btnCarregarConfiguracoes.TabIndex = 1;
            this.btnCarregarConfiguracoes.Text = "Carregar Configurações";
            this.btnCarregarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnCarregarConfiguracoes.Click += new System.EventHandler(this.btnCarregarConfiguracoes_Click);
            // 
            // btnSalvarConfiguracoes
            // 
            this.btnSalvarConfiguracoes.Location = new System.Drawing.Point(6, 188);
            this.btnSalvarConfiguracoes.Name = "btnSalvarConfiguracoes";
            this.btnSalvarConfiguracoes.Size = new System.Drawing.Size(166, 34);
            this.btnSalvarConfiguracoes.TabIndex = 0;
            this.btnSalvarConfiguracoes.Text = "Salvar Configurações";
            this.btnSalvarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnSalvarConfiguracoes.Click += new System.EventHandler(this.btnSalvarConfiguracoes_Click);
            // 
            // tPageBuscarCEP
            // 
            this.tPageBuscarCEP.Controls.Add(this.grpBoxBuscarPorLogradouro);
            this.tPageBuscarCEP.Controls.Add(this.grpBoxBuscarPorCEP);
            this.tPageBuscarCEP.Location = new System.Drawing.Point(4, 22);
            this.tPageBuscarCEP.Name = "tPageBuscarCEP";
            this.tPageBuscarCEP.Padding = new System.Windows.Forms.Padding(3);
            this.tPageBuscarCEP.Size = new System.Drawing.Size(595, 228);
            this.tPageBuscarCEP.TabIndex = 1;
            this.tPageBuscarCEP.Text = "Buscar CEP";
            this.tPageBuscarCEP.UseVisualStyleBackColor = true;
            // 
            // grpBoxBuscarPorLogradouro
            // 
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.txtBairro);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.lblBairro);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.txtUF);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.lblUF);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.txtCidade);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.lblCidade);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.txtTipoLogradouro);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.lblTipoEndereco);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.btnBuscarPorLogradouro);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.txtLogradouro);
            this.grpBoxBuscarPorLogradouro.Controls.Add(this.lblLogradouro);
            this.grpBoxBuscarPorLogradouro.Location = new System.Drawing.Point(279, 6);
            this.grpBoxBuscarPorLogradouro.Name = "grpBoxBuscarPorLogradouro";
            this.grpBoxBuscarPorLogradouro.Size = new System.Drawing.Size(310, 141);
            this.grpBoxBuscarPorLogradouro.TabIndex = 18;
            this.grpBoxBuscarPorLogradouro.TabStop = false;
            this.grpBoxBuscarPorLogradouro.Text = "Por Endereço";
            // 
            // txtBairro
            // 
            this.txtBairro.Location = new System.Drawing.Point(161, 100);
            this.txtBairro.Name = "txtBairro";
            this.txtBairro.Size = new System.Drawing.Size(80, 20);
            this.txtBairro.TabIndex = 21;
            // 
            // lblBairro
            // 
            this.lblBairro.AutoSize = true;
            this.lblBairro.Location = new System.Drawing.Point(161, 84);
            this.lblBairro.Name = "lblBairro";
            this.lblBairro.Size = new System.Drawing.Size(34, 13);
            this.lblBairro.TabIndex = 20;
            this.lblBairro.Text = "Bairro";
            // 
            // txtUF
            // 
            this.txtUF.Location = new System.Drawing.Point(117, 100);
            this.txtUF.Name = "txtUF";
            this.txtUF.Size = new System.Drawing.Size(38, 20);
            this.txtUF.TabIndex = 19;
            // 
            // lblUF
            // 
            this.lblUF.AutoSize = true;
            this.lblUF.Location = new System.Drawing.Point(114, 84);
            this.lblUF.Name = "lblUF";
            this.lblUF.Size = new System.Drawing.Size(21, 13);
            this.lblUF.TabIndex = 18;
            this.lblUF.Text = "UF";
            // 
            // txtCidade
            // 
            this.txtCidade.Location = new System.Drawing.Point(6, 100);
            this.txtCidade.Name = "txtCidade";
            this.txtCidade.Size = new System.Drawing.Size(105, 20);
            this.txtCidade.TabIndex = 17;
            // 
            // lblCidade
            // 
            this.lblCidade.AutoSize = true;
            this.lblCidade.Location = new System.Drawing.Point(3, 84);
            this.lblCidade.Name = "lblCidade";
            this.lblCidade.Size = new System.Drawing.Size(40, 13);
            this.lblCidade.TabIndex = 16;
            this.lblCidade.Text = "Cidade";
            // 
            // txtTipoLogradouro
            // 
            this.txtTipoLogradouro.Location = new System.Drawing.Point(6, 48);
            this.txtTipoLogradouro.Name = "txtTipoLogradouro";
            this.txtTipoLogradouro.Size = new System.Drawing.Size(60, 20);
            this.txtTipoLogradouro.TabIndex = 15;
            // 
            // lblTipoEndereco
            // 
            this.lblTipoEndereco.AutoSize = true;
            this.lblTipoEndereco.Location = new System.Drawing.Point(3, 32);
            this.lblTipoEndereco.Name = "lblTipoEndereco";
            this.lblTipoEndereco.Size = new System.Drawing.Size(28, 13);
            this.lblTipoEndereco.TabIndex = 14;
            this.lblTipoEndereco.Text = "Tipo";
            // 
            // btnBuscarPorLogradouro
            // 
            this.btnBuscarPorLogradouro.Location = new System.Drawing.Point(247, 46);
            this.btnBuscarPorLogradouro.Name = "btnBuscarPorLogradouro";
            this.btnBuscarPorLogradouro.Size = new System.Drawing.Size(57, 74);
            this.btnBuscarPorLogradouro.TabIndex = 11;
            this.btnBuscarPorLogradouro.Text = "Buscar";
            this.btnBuscarPorLogradouro.UseVisualStyleBackColor = true;
            this.btnBuscarPorLogradouro.Click += new System.EventHandler(this.btnBuscarPorLogradouro_Click);
            // 
            // txtLogradouro
            // 
            this.txtLogradouro.Location = new System.Drawing.Point(72, 48);
            this.txtLogradouro.Name = "txtLogradouro";
            this.txtLogradouro.Size = new System.Drawing.Size(169, 20);
            this.txtLogradouro.TabIndex = 13;
            // 
            // lblLogradouro
            // 
            this.lblLogradouro.AutoSize = true;
            this.lblLogradouro.Location = new System.Drawing.Point(69, 32);
            this.lblLogradouro.Name = "lblLogradouro";
            this.lblLogradouro.Size = new System.Drawing.Size(61, 13);
            this.lblLogradouro.TabIndex = 12;
            this.lblLogradouro.Text = "Logradouro";
            // 
            // grpBoxBuscarPorCEP
            // 
            this.grpBoxBuscarPorCEP.Controls.Add(this.btnBuscarPorCEP);
            this.grpBoxBuscarPorCEP.Controls.Add(this.txtCEP);
            this.grpBoxBuscarPorCEP.Controls.Add(this.lblCEP);
            this.grpBoxBuscarPorCEP.Location = new System.Drawing.Point(6, 6);
            this.grpBoxBuscarPorCEP.Name = "grpBoxBuscarPorCEP";
            this.grpBoxBuscarPorCEP.Size = new System.Drawing.Size(267, 141);
            this.grpBoxBuscarPorCEP.TabIndex = 17;
            this.grpBoxBuscarPorCEP.TabStop = false;
            this.grpBoxBuscarPorCEP.Text = "Por CEP";
            // 
            // btnBuscarPorCEP
            // 
            this.btnBuscarPorCEP.Location = new System.Drawing.Point(62, 72);
            this.btnBuscarPorCEP.Name = "btnBuscarPorCEP";
            this.btnBuscarPorCEP.Size = new System.Drawing.Size(124, 34);
            this.btnBuscarPorCEP.TabIndex = 8;
            this.btnBuscarPorCEP.Text = "Buscar";
            this.btnBuscarPorCEP.UseVisualStyleBackColor = true;
            this.btnBuscarPorCEP.Click += new System.EventHandler(this.btnBuscarPorCEP_Click);
            // 
            // txtCEP
            // 
            this.txtCEP.Location = new System.Drawing.Point(62, 46);
            this.txtCEP.Name = "txtCEP";
            this.txtCEP.Size = new System.Drawing.Size(124, 20);
            this.txtCEP.TabIndex = 10;
            // 
            // lblCEP
            // 
            this.lblCEP.AutoSize = true;
            this.lblCEP.Location = new System.Drawing.Point(59, 30);
            this.lblCEP.Name = "lblCEP";
            this.lblCEP.Size = new System.Drawing.Size(28, 13);
            this.lblCEP.TabIndex = 9;
            this.lblCEP.Text = "CEP";
            // 
            // txtRetorno
            // 
            this.txtRetorno.Location = new System.Drawing.Point(12, 272);
            this.txtRetorno.Multiline = true;
            this.txtRetorno.Name = "txtRetorno";
            this.txtRetorno.Size = new System.Drawing.Size(603, 125);
            this.txtRetorno.TabIndex = 19;
            // 
            // chkPesquisarIBGE
            // 
            this.chkPesquisarIBGE.AutoSize = true;
            this.chkPesquisarIBGE.Location = new System.Drawing.Point(6, 46);
            this.chkPesquisarIBGE.Name = "chkPesquisarIBGE";
            this.chkPesquisarIBGE.Size = new System.Drawing.Size(100, 17);
            this.chkPesquisarIBGE.TabIndex = 12;
            this.chkPesquisarIBGE.Text = "Pesquisar IBGE";
            this.chkPesquisarIBGE.UseVisualStyleBackColor = true;
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(624, 408);
            this.Controls.Add(this.txtRetorno);
            this.Controls.Add(this.tabControl1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibCEP Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.tabControl1.ResumeLayout(false);
            this.tPageConfiguracao.ResumeLayout(false);
            this.grpBoxProxy.ResumeLayout(false);
            this.grpBoxProxy.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudProxyPorta)).EndInit();
            this.grpBoxWebService.ResumeLayout(false);
            this.grpBoxWebService.PerformLayout();
            this.tPageBuscarCEP.ResumeLayout(false);
            this.grpBoxBuscarPorLogradouro.ResumeLayout(false);
            this.grpBoxBuscarPorLogradouro.PerformLayout();
            this.grpBoxBuscarPorCEP.ResumeLayout(false);
            this.grpBoxBuscarPorCEP.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tPageConfiguracao;
        private System.Windows.Forms.TabPage tPageBuscarCEP;
        private System.Windows.Forms.TextBox txtRetorno;
        private System.Windows.Forms.Button btnSalvarConfiguracoes;
        private System.Windows.Forms.Button btnCarregarConfiguracoes;
        private System.Windows.Forms.GroupBox grpBoxProxy;
        private System.Windows.Forms.GroupBox grpBoxWebService;
        private System.Windows.Forms.GroupBox grpBoxBuscarPorLogradouro;
        private System.Windows.Forms.GroupBox grpBoxBuscarPorCEP;
        private System.Windows.Forms.TextBox txtChaveWebService;
        private System.Windows.Forms.Label lblChaveWebService;
        private System.Windows.Forms.TextBox txtSenhaWebService;
        private System.Windows.Forms.Label lblSenhaWebService;
        private System.Windows.Forms.TextBox txtUsuarioWebService;
        private System.Windows.Forms.Label lblUsuarioWebService;
        private System.Windows.Forms.Label lblPortaProxy;
        private System.Windows.Forms.TextBox txtHostProxy;
        private System.Windows.Forms.Label lblHostProxy;
        private System.Windows.Forms.TextBox txtSenhaProxy;
        private System.Windows.Forms.Label lblSenhaProxy;
        private System.Windows.Forms.TextBox txtUsuarioProxy;
        private System.Windows.Forms.Label lblUsuarioProxy;
        private System.Windows.Forms.Button btnBuscarPorCEP;
        private System.Windows.Forms.TextBox txtCEP;
        private System.Windows.Forms.Label lblCEP;
        private System.Windows.Forms.Button btnBuscarPorLogradouro;
        private System.Windows.Forms.TextBox txtLogradouro;
        private System.Windows.Forms.Label lblLogradouro;
        private System.Windows.Forms.TextBox txtTipoLogradouro;
        private System.Windows.Forms.Label lblTipoEndereco;
        private System.Windows.Forms.TextBox txtBairro;
        private System.Windows.Forms.Label lblBairro;
        private System.Windows.Forms.TextBox txtUF;
        private System.Windows.Forms.Label lblUF;
        private System.Windows.Forms.TextBox txtCidade;
        private System.Windows.Forms.Label lblCidade;
        private System.Windows.Forms.ComboBox cmbWebService;
        private System.Windows.Forms.NumericUpDown nudProxyPorta;
        private System.Windows.Forms.CheckBox chkPesquisarIBGE;
    }
}

