namespace ACBrLibIBGE.Demo
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
            this.txtRetorno = new System.Windows.Forms.TextBox();
            this.grpBoxBuscarPorNome = new System.Windows.Forms.GroupBox();
            this.chkBoxIgnorarCaixaseAcentos = new System.Windows.Forms.CheckBox();
            this.txtUF = new System.Windows.Forms.TextBox();
            this.lblUF = new System.Windows.Forms.Label();
            this.btnBuscarPorNome = new System.Windows.Forms.Button();
            this.txtCidade = new System.Windows.Forms.TextBox();
            this.lblCidade = new System.Windows.Forms.Label();
            this.btnSalvarConfiguracoes = new System.Windows.Forms.Button();
            this.btnCarregarConfiguracoes = new System.Windows.Forms.Button();
            this.grpBoxBuscarPorCodigo = new System.Windows.Forms.GroupBox();
            this.btnBuscarPorCodigo = new System.Windows.Forms.Button();
            this.txtCodMunicipio = new System.Windows.Forms.TextBox();
            this.lblCodMunicipio = new System.Windows.Forms.Label();
            this.grpBoxBuscarPorNome.SuspendLayout();
            this.grpBoxBuscarPorCodigo.SuspendLayout();
            this.SuspendLayout();
            // 
            // txtRetorno
            // 
            this.txtRetorno.Location = new System.Drawing.Point(9, 167);
            this.txtRetorno.Multiline = true;
            this.txtRetorno.Name = "txtRetorno";
            this.txtRetorno.Size = new System.Drawing.Size(483, 125);
            this.txtRetorno.TabIndex = 19;
            // 
            // grpBoxBuscarPorNome
            // 
            this.grpBoxBuscarPorNome.Controls.Add(this.chkBoxIgnorarCaixaseAcentos);
            this.grpBoxBuscarPorNome.Controls.Add(this.txtUF);
            this.grpBoxBuscarPorNome.Controls.Add(this.lblUF);
            this.grpBoxBuscarPorNome.Controls.Add(this.btnBuscarPorNome);
            this.grpBoxBuscarPorNome.Controls.Add(this.txtCidade);
            this.grpBoxBuscarPorNome.Controls.Add(this.lblCidade);
            this.grpBoxBuscarPorNome.Location = new System.Drawing.Point(181, 12);
            this.grpBoxBuscarPorNome.Name = "grpBoxBuscarPorNome";
            this.grpBoxBuscarPorNome.Size = new System.Drawing.Size(310, 121);
            this.grpBoxBuscarPorNome.TabIndex = 21;
            this.grpBoxBuscarPorNome.TabStop = false;
            this.grpBoxBuscarPorNome.Text = "Por Nome";
            // 
            // chkBoxIgnorarCaixaseAcentos
            // 
            this.chkBoxIgnorarCaixaseAcentos.AutoSize = true;
            this.chkBoxIgnorarCaixaseAcentos.Location = new System.Drawing.Point(83, 76);
            this.chkBoxIgnorarCaixaseAcentos.Name = "chkBoxIgnorarCaixaseAcentos";
            this.chkBoxIgnorarCaixaseAcentos.Size = new System.Drawing.Size(144, 17);
            this.chkBoxIgnorarCaixaseAcentos.TabIndex = 16;
            this.chkBoxIgnorarCaixaseAcentos.Text = "Ignorar Caixas e Acentos";
            this.chkBoxIgnorarCaixaseAcentos.UseVisualStyleBackColor = true;
            // 
            // txtUF
            // 
            this.txtUF.Location = new System.Drawing.Point(241, 44);
            this.txtUF.Name = "txtUF";
            this.txtUF.Size = new System.Drawing.Size(60, 20);
            this.txtUF.TabIndex = 15;
            // 
            // lblUF
            // 
            this.lblUF.AutoSize = true;
            this.lblUF.Location = new System.Drawing.Point(238, 28);
            this.lblUF.Name = "lblUF";
            this.lblUF.Size = new System.Drawing.Size(21, 13);
            this.lblUF.TabIndex = 14;
            this.lblUF.Text = "UF";
            // 
            // btnBuscarPorNome
            // 
            this.btnBuscarPorNome.Location = new System.Drawing.Point(6, 70);
            this.btnBuscarPorNome.Name = "btnBuscarPorNome";
            this.btnBuscarPorNome.Size = new System.Drawing.Size(71, 26);
            this.btnBuscarPorNome.TabIndex = 11;
            this.btnBuscarPorNome.Text = "Buscar";
            this.btnBuscarPorNome.UseVisualStyleBackColor = true;
            this.btnBuscarPorNome.Click += new System.EventHandler(this.btnBuscarPorNome_Click);
            // 
            // txtCidade
            // 
            this.txtCidade.Location = new System.Drawing.Point(6, 44);
            this.txtCidade.Name = "txtCidade";
            this.txtCidade.Size = new System.Drawing.Size(229, 20);
            this.txtCidade.TabIndex = 13;
            // 
            // lblCidade
            // 
            this.lblCidade.AutoSize = true;
            this.lblCidade.Location = new System.Drawing.Point(3, 28);
            this.lblCidade.Name = "lblCidade";
            this.lblCidade.Size = new System.Drawing.Size(40, 13);
            this.lblCidade.TabIndex = 12;
            this.lblCidade.Text = "Cidade";
            // 
            // btnSalvarConfiguracoes
            // 
            this.btnSalvarConfiguracoes.Location = new System.Drawing.Point(9, 139);
            this.btnSalvarConfiguracoes.Name = "btnSalvarConfiguracoes";
            this.btnSalvarConfiguracoes.Size = new System.Drawing.Size(166, 22);
            this.btnSalvarConfiguracoes.TabIndex = 22;
            this.btnSalvarConfiguracoes.Text = "Salvar Configurações";
            this.btnSalvarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnSalvarConfiguracoes.Click += new System.EventHandler(this.btnSalvarConfiguracoes_Click_1);
            // 
            // btnCarregarConfiguracoes
            // 
            this.btnCarregarConfiguracoes.Location = new System.Drawing.Point(181, 139);
            this.btnCarregarConfiguracoes.Name = "btnCarregarConfiguracoes";
            this.btnCarregarConfiguracoes.Size = new System.Drawing.Size(166, 22);
            this.btnCarregarConfiguracoes.TabIndex = 23;
            this.btnCarregarConfiguracoes.Text = "Carregar Configurações";
            this.btnCarregarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnCarregarConfiguracoes.Click += new System.EventHandler(this.btnCarregarConfiguracoes_Click_1);
            // 
            // grpBoxBuscarPorCodigo
            // 
            this.grpBoxBuscarPorCodigo.Controls.Add(this.btnBuscarPorCodigo);
            this.grpBoxBuscarPorCodigo.Controls.Add(this.txtCodMunicipio);
            this.grpBoxBuscarPorCodigo.Controls.Add(this.lblCodMunicipio);
            this.grpBoxBuscarPorCodigo.Location = new System.Drawing.Point(8, 12);
            this.grpBoxBuscarPorCodigo.Name = "grpBoxBuscarPorCodigo";
            this.grpBoxBuscarPorCodigo.Size = new System.Drawing.Size(166, 121);
            this.grpBoxBuscarPorCodigo.TabIndex = 24;
            this.grpBoxBuscarPorCodigo.TabStop = false;
            this.grpBoxBuscarPorCodigo.Text = "Por Código";
            // 
            // btnBuscarPorCodigo
            // 
            this.btnBuscarPorCodigo.Location = new System.Drawing.Point(23, 70);
            this.btnBuscarPorCodigo.Name = "btnBuscarPorCodigo";
            this.btnBuscarPorCodigo.Size = new System.Drawing.Size(124, 26);
            this.btnBuscarPorCodigo.TabIndex = 8;
            this.btnBuscarPorCodigo.Text = "Buscar";
            this.btnBuscarPorCodigo.UseVisualStyleBackColor = true;
            this.btnBuscarPorCodigo.Click += new System.EventHandler(this.btnBuscarPorCodigo_Click);
            // 
            // txtCodMunicipio
            // 
            this.txtCodMunicipio.Location = new System.Drawing.Point(23, 44);
            this.txtCodMunicipio.Name = "txtCodMunicipio";
            this.txtCodMunicipio.Size = new System.Drawing.Size(124, 20);
            this.txtCodMunicipio.TabIndex = 10;
            // 
            // lblCodMunicipio
            // 
            this.lblCodMunicipio.AutoSize = true;
            this.lblCodMunicipio.Location = new System.Drawing.Point(20, 28);
            this.lblCodMunicipio.Name = "lblCodMunicipio";
            this.lblCodMunicipio.Size = new System.Drawing.Size(79, 13);
            this.lblCodMunicipio.TabIndex = 9;
            this.lblCodMunicipio.Text = "Cod. Município";
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(501, 300);
            this.Controls.Add(this.grpBoxBuscarPorCodigo);
            this.Controls.Add(this.btnCarregarConfiguracoes);
            this.Controls.Add(this.btnSalvarConfiguracoes);
            this.Controls.Add(this.grpBoxBuscarPorNome);
            this.Controls.Add(this.txtRetorno);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibIBGE Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.grpBoxBuscarPorNome.ResumeLayout(false);
            this.grpBoxBuscarPorNome.PerformLayout();
            this.grpBoxBuscarPorCodigo.ResumeLayout(false);
            this.grpBoxBuscarPorCodigo.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.TextBox txtRetorno;
        private System.Windows.Forms.GroupBox grpBoxBuscarPorNome;
        private System.Windows.Forms.TextBox txtUF;
        private System.Windows.Forms.Label lblUF;
        private System.Windows.Forms.Button btnBuscarPorNome;
        private System.Windows.Forms.TextBox txtCidade;
        private System.Windows.Forms.Label lblCidade;
        private System.Windows.Forms.Button btnSalvarConfiguracoes;
        private System.Windows.Forms.Button btnCarregarConfiguracoes;
        private System.Windows.Forms.GroupBox grpBoxBuscarPorCodigo;
        private System.Windows.Forms.Button btnBuscarPorCodigo;
        private System.Windows.Forms.TextBox txtCodMunicipio;
        private System.Windows.Forms.Label lblCodMunicipio;
        private System.Windows.Forms.CheckBox chkBoxIgnorarCaixaseAcentos;
    }
}

