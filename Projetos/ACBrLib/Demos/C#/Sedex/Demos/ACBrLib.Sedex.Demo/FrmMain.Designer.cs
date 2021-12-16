namespace ACBrLibSedex.Demo
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
            this.btnSalvarConfiguracoes = new System.Windows.Forms.Button();
            this.btnCarregarConfiguracoes = new System.Windows.Forms.Button();
            this.txtCodRastreio = new System.Windows.Forms.TextBox();
            this.lblCodRastreio = new System.Windows.Forms.Label();
            this.btnConsultar = new System.Windows.Forms.Button();
            this.txtSenha = new System.Windows.Forms.TextBox();
            this.lblSenha = new System.Windows.Forms.Label();
            this.btnRastrear = new System.Windows.Forms.Button();
            this.txtCodContrato = new System.Windows.Forms.TextBox();
            this.lblCodContrato = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // txtRetorno
            // 
            this.txtRetorno.Location = new System.Drawing.Point(8, 374);
            this.txtRetorno.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.txtRetorno.Multiline = true;
            this.txtRetorno.Name = "txtRetorno";
            this.txtRetorno.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.txtRetorno.Size = new System.Drawing.Size(436, 379);
            this.txtRetorno.TabIndex = 19;
            // 
            // btnSalvarConfiguracoes
            // 
            this.btnSalvarConfiguracoes.Location = new System.Drawing.Point(8, 311);
            this.btnSalvarConfiguracoes.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.btnSalvarConfiguracoes.Name = "btnSalvarConfiguracoes";
            this.btnSalvarConfiguracoes.Size = new System.Drawing.Size(201, 34);
            this.btnSalvarConfiguracoes.TabIndex = 22;
            this.btnSalvarConfiguracoes.Text = "Salvar Configurações";
            this.btnSalvarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnSalvarConfiguracoes.Click += new System.EventHandler(this.btnSalvarConfiguracoes_Click_1);
            // 
            // btnCarregarConfiguracoes
            // 
            this.btnCarregarConfiguracoes.Location = new System.Drawing.Point(244, 311);
            this.btnCarregarConfiguracoes.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.btnCarregarConfiguracoes.Name = "btnCarregarConfiguracoes";
            this.btnCarregarConfiguracoes.Size = new System.Drawing.Size(201, 34);
            this.btnCarregarConfiguracoes.TabIndex = 23;
            this.btnCarregarConfiguracoes.Text = "Carregar Configurações";
            this.btnCarregarConfiguracoes.UseVisualStyleBackColor = true;
            this.btnCarregarConfiguracoes.Click += new System.EventHandler(this.btnCarregarConfiguracoes_Click_1);
            // 
            // txtCodRastreio
            // 
            this.txtCodRastreio.Location = new System.Drawing.Point(8, 146);
            this.txtCodRastreio.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.txtCodRastreio.Name = "txtCodRastreio";
            this.txtCodRastreio.Size = new System.Drawing.Size(199, 26);
            this.txtCodRastreio.TabIndex = 29;
            // 
            // lblCodRastreio
            // 
            this.lblCodRastreio.AutoSize = true;
            this.lblCodRastreio.Location = new System.Drawing.Point(3, 122);
            this.lblCodRastreio.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblCodRastreio.Name = "lblCodRastreio";
            this.lblCodRastreio.Size = new System.Drawing.Size(124, 20);
            this.lblCodRastreio.TabIndex = 28;
            this.lblCodRastreio.Text = "Cod de Rastreio";
            // 
            // btnConsultar
            // 
            this.btnConsultar.Location = new System.Drawing.Point(244, 208);
            this.btnConsultar.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.btnConsultar.Name = "btnConsultar";
            this.btnConsultar.Size = new System.Drawing.Size(201, 40);
            this.btnConsultar.TabIndex = 25;
            this.btnConsultar.Text = "Consultar";
            this.btnConsultar.UseVisualStyleBackColor = true;
            this.btnConsultar.Click += new System.EventHandler(this.btnConsultar_Click);
            // 
            // txtSenha
            // 
            this.txtSenha.Location = new System.Drawing.Point(244, 62);
            this.txtSenha.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.txtSenha.Name = "txtSenha";
            this.txtSenha.Size = new System.Drawing.Size(199, 26);
            this.txtSenha.TabIndex = 27;
            this.txtSenha.UseSystemPasswordChar = true;
            // 
            // lblSenha
            // 
            this.lblSenha.AutoSize = true;
            this.lblSenha.Location = new System.Drawing.Point(240, 37);
            this.lblSenha.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblSenha.Name = "lblSenha";
            this.lblSenha.Size = new System.Drawing.Size(56, 20);
            this.lblSenha.TabIndex = 26;
            this.lblSenha.Text = "Senha";
            // 
            // btnRastrear
            // 
            this.btnRastrear.Location = new System.Drawing.Point(8, 208);
            this.btnRastrear.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.btnRastrear.Name = "btnRastrear";
            this.btnRastrear.Size = new System.Drawing.Size(201, 40);
            this.btnRastrear.TabIndex = 31;
            this.btnRastrear.Text = "Rastrear";
            this.btnRastrear.UseVisualStyleBackColor = true;
            this.btnRastrear.Click += new System.EventHandler(this.btnRastrear_Click);
            // 
            // txtCodContrato
            // 
            this.txtCodContrato.Location = new System.Drawing.Point(8, 62);
            this.txtCodContrato.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.txtCodContrato.Name = "txtCodContrato";
            this.txtCodContrato.Size = new System.Drawing.Size(199, 26);
            this.txtCodContrato.TabIndex = 33;
            // 
            // lblCodContrato
            // 
            this.lblCodContrato.AutoSize = true;
            this.lblCodContrato.Location = new System.Drawing.Point(3, 37);
            this.lblCodContrato.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblCodContrato.Name = "lblCodContrato";
            this.lblCodContrato.Size = new System.Drawing.Size(130, 20);
            this.lblCodContrato.TabIndex = 32;
            this.lblCodContrato.Text = "Cod. do Contrato";
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(465, 774);
            this.Controls.Add(this.btnRastrear);
            this.Controls.Add(this.txtCodContrato);
            this.Controls.Add(this.lblCodContrato);
            this.Controls.Add(this.txtCodRastreio);
            this.Controls.Add(this.lblCodRastreio);
            this.Controls.Add(this.btnConsultar);
            this.Controls.Add(this.txtSenha);
            this.Controls.Add(this.lblSenha);
            this.Controls.Add(this.btnCarregarConfiguracoes);
            this.Controls.Add(this.btnSalvarConfiguracoes);
            this.Controls.Add(this.txtRetorno);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibSedex Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.TextBox txtRetorno;
        private System.Windows.Forms.Button btnSalvarConfiguracoes;
        private System.Windows.Forms.Button btnCarregarConfiguracoes;
        private System.Windows.Forms.TextBox txtCodRastreio;
        private System.Windows.Forms.Label lblCodRastreio;
        private System.Windows.Forms.Button btnConsultar;
        private System.Windows.Forms.TextBox txtSenha;
        private System.Windows.Forms.Label lblSenha;
        private System.Windows.Forms.Button btnRastrear;
        private System.Windows.Forms.TextBox txtCodContrato;
        private System.Windows.Forms.Label lblCodContrato;
    }
}

