namespace ACBrLibPosPrinter.Demo
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
            this.label8 = new System.Windows.Forms.Label();
            this.txtArqLog = new System.Windows.Forms.TextBox();
            this.btnArqLog = new System.Windows.Forms.Button();
            this.cbbPaginaCodigo = new System.Windows.Forms.ComboBox();
            this.label7 = new System.Windows.Forms.Label();
            this.cbxIgnorarTags = new System.Windows.Forms.CheckBox();
            this.cbxTraduzirTags = new System.Windows.Forms.CheckBox();
            this.cbxCortarPapel = new System.Windows.Forms.CheckBox();
            this.cbxControlePorta = new System.Windows.Forms.CheckBox();
            this.label6 = new System.Windows.Forms.Label();
            this.nudLinhasPular = new System.Windows.Forms.NumericUpDown();
            this.label5 = new System.Windows.Forms.Label();
            this.nudBuffer = new System.Windows.Forms.NumericUpDown();
            this.label4 = new System.Windows.Forms.Label();
            this.nudEspacos = new System.Windows.Forms.NumericUpDown();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.nudColunas = new System.Windows.Forms.NumericUpDown();
            this.cbbPortas = new System.Windows.Forms.ComboBox();
            this.cbbModelo = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.btnAtivar = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnAddTags = new System.Windows.Forms.Button();
            this.btnLimpar = new System.Windows.Forms.Button();
            this.btnImprimir = new System.Windows.Forms.Button();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.txtImpressao = new System.Windows.Forms.TextBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudLinhasPular)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudBuffer)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudEspacos)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudColunas)).BeginInit();
            this.panel1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.label8);
            this.groupBox1.Controls.Add(this.txtArqLog);
            this.groupBox1.Controls.Add(this.btnArqLog);
            this.groupBox1.Controls.Add(this.cbbPaginaCodigo);
            this.groupBox1.Controls.Add(this.label7);
            this.groupBox1.Controls.Add(this.cbxIgnorarTags);
            this.groupBox1.Controls.Add(this.cbxTraduzirTags);
            this.groupBox1.Controls.Add(this.cbxCortarPapel);
            this.groupBox1.Controls.Add(this.cbxControlePorta);
            this.groupBox1.Controls.Add(this.label6);
            this.groupBox1.Controls.Add(this.nudLinhasPular);
            this.groupBox1.Controls.Add(this.label5);
            this.groupBox1.Controls.Add(this.nudBuffer);
            this.groupBox1.Controls.Add(this.label4);
            this.groupBox1.Controls.Add(this.nudEspacos);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.nudColunas);
            this.groupBox1.Controls.Add(this.cbbPortas);
            this.groupBox1.Controls.Add(this.cbbModelo);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.btnAtivar);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(225, 269);
            this.groupBox1.TabIndex = 1;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Configuração";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(108, 167);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(47, 13);
            this.label8.TabIndex = 26;
            this.label8.Text = "Arq. Log";
            // 
            // txtArqLog
            // 
            this.txtArqLog.Location = new System.Drawing.Point(111, 183);
            this.txtArqLog.Name = "txtArqLog";
            this.txtArqLog.Size = new System.Drawing.Size(85, 20);
            this.txtArqLog.TabIndex = 24;
            this.txtArqLog.Text = "PosPrinter.log";
            // 
            // btnArqLog
            // 
            this.btnArqLog.Location = new System.Drawing.Point(195, 182);
            this.btnArqLog.Name = "btnArqLog";
            this.btnArqLog.Size = new System.Drawing.Size(24, 22);
            this.btnArqLog.TabIndex = 25;
            this.btnArqLog.Text = "...";
            this.btnArqLog.UseVisualStyleBackColor = true;
            this.btnArqLog.Click += new System.EventHandler(this.btnArqLog_Click);
            // 
            // cbbPaginaCodigo
            // 
            this.cbbPaginaCodigo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbbPaginaCodigo.FormattingEnabled = true;
            this.cbbPaginaCodigo.Location = new System.Drawing.Point(111, 229);
            this.cbbPaginaCodigo.Name = "cbbPaginaCodigo";
            this.cbbPaginaCodigo.Size = new System.Drawing.Size(108, 21);
            this.cbbPaginaCodigo.TabIndex = 23;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(111, 213);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(65, 13);
            this.label7.TabIndex = 22;
            this.label7.Text = "Pag. Código";
            // 
            // cbxIgnorarTags
            // 
            this.cbxIgnorarTags.AutoSize = true;
            this.cbxIgnorarTags.Location = new System.Drawing.Point(6, 233);
            this.cbxIgnorarTags.Name = "cbxIgnorarTags";
            this.cbxIgnorarTags.Size = new System.Drawing.Size(86, 17);
            this.cbxIgnorarTags.TabIndex = 21;
            this.cbxIgnorarTags.Text = "Ignorar Tags";
            this.cbxIgnorarTags.UseVisualStyleBackColor = true;
            // 
            // cbxTraduzirTags
            // 
            this.cbxTraduzirTags.AutoSize = true;
            this.cbxTraduzirTags.Checked = true;
            this.cbxTraduzirTags.CheckState = System.Windows.Forms.CheckState.Checked;
            this.cbxTraduzirTags.Location = new System.Drawing.Point(6, 210);
            this.cbxTraduzirTags.Name = "cbxTraduzirTags";
            this.cbxTraduzirTags.Size = new System.Drawing.Size(91, 17);
            this.cbxTraduzirTags.TabIndex = 20;
            this.cbxTraduzirTags.Text = "Traduzir Tags";
            this.cbxTraduzirTags.UseVisualStyleBackColor = true;
            // 
            // cbxCortarPapel
            // 
            this.cbxCortarPapel.AutoSize = true;
            this.cbxCortarPapel.Checked = true;
            this.cbxCortarPapel.CheckState = System.Windows.Forms.CheckState.Checked;
            this.cbxCortarPapel.Location = new System.Drawing.Point(6, 187);
            this.cbxCortarPapel.Name = "cbxCortarPapel";
            this.cbxCortarPapel.Size = new System.Drawing.Size(84, 17);
            this.cbxCortarPapel.TabIndex = 19;
            this.cbxCortarPapel.Text = "Cortar Papel";
            this.cbxCortarPapel.UseVisualStyleBackColor = true;
            // 
            // cbxControlePorta
            // 
            this.cbxControlePorta.AutoSize = true;
            this.cbxControlePorta.Location = new System.Drawing.Point(6, 164);
            this.cbxControlePorta.Name = "cbxControlePorta";
            this.cbxControlePorta.Size = new System.Drawing.Size(93, 17);
            this.cbxControlePorta.TabIndex = 18;
            this.cbxControlePorta.Text = "Controle Porta";
            this.cbxControlePorta.UseVisualStyleBackColor = true;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(157, 109);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(65, 13);
            this.label6.TabIndex = 17;
            this.label6.Text = "Linhas Pular";
            // 
            // nudLinhasPular
            // 
            this.nudLinhasPular.Location = new System.Drawing.Point(159, 125);
            this.nudLinhasPular.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudLinhasPular.Name = "nudLinhasPular";
            this.nudLinhasPular.Size = new System.Drawing.Size(60, 20);
            this.nudLinhasPular.TabIndex = 16;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(108, 109);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(35, 13);
            this.label5.TabIndex = 15;
            this.label5.Text = "Buffer";
            // 
            // nudBuffer
            // 
            this.nudBuffer.Location = new System.Drawing.Point(108, 125);
            this.nudBuffer.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudBuffer.Name = "nudBuffer";
            this.nudBuffer.Size = new System.Drawing.Size(45, 20);
            this.nudBuffer.TabIndex = 14;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(57, 109);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(48, 13);
            this.label4.TabIndex = 13;
            this.label4.Text = "Espaços";
            // 
            // nudEspacos
            // 
            this.nudEspacos.Location = new System.Drawing.Point(57, 125);
            this.nudEspacos.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudEspacos.Name = "nudEspacos";
            this.nudEspacos.Size = new System.Drawing.Size(45, 20);
            this.nudEspacos.TabIndex = 12;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(6, 109);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(45, 13);
            this.label3.TabIndex = 11;
            this.label3.Text = "Colunas";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(3, 59);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(32, 13);
            this.label2.TabIndex = 9;
            this.label2.Text = "Porta";
            // 
            // nudColunas
            // 
            this.nudColunas.Location = new System.Drawing.Point(6, 125);
            this.nudColunas.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.nudColunas.Name = "nudColunas";
            this.nudColunas.Size = new System.Drawing.Size(45, 20);
            this.nudColunas.TabIndex = 6;
            // 
            // cbbPortas
            // 
            this.cbbPortas.FormattingEnabled = true;
            this.cbbPortas.Location = new System.Drawing.Point(6, 75);
            this.cbbPortas.Name = "cbbPortas";
            this.cbbPortas.Size = new System.Drawing.Size(213, 21);
            this.cbbPortas.TabIndex = 8;
            // 
            // cbbModelo
            // 
            this.cbbModelo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbbModelo.FormattingEnabled = true;
            this.cbbModelo.Location = new System.Drawing.Point(6, 35);
            this.cbbModelo.Name = "cbbModelo";
            this.cbbModelo.Size = new System.Drawing.Size(107, 21);
            this.cbbModelo.TabIndex = 7;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 19);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(42, 13);
            this.label1.TabIndex = 6;
            this.label1.Text = "Modelo";
            // 
            // btnAtivar
            // 
            this.btnAtivar.Font = new System.Drawing.Font("Microsoft Sans Serif", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnAtivar.Image = global::ACBrLibPosPrinter.Demo.Properties.Resources.ativar;
            this.btnAtivar.Location = new System.Drawing.Point(119, 12);
            this.btnAtivar.Name = "btnAtivar";
            this.btnAtivar.Size = new System.Drawing.Size(100, 59);
            this.btnAtivar.TabIndex = 5;
            this.btnAtivar.Text = "Ativar";
            this.btnAtivar.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnAtivar.UseVisualStyleBackColor = true;
            this.btnAtivar.Click += new System.EventHandler(this.btnAtivar_Click);
            // 
            // panel1
            // 
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel1.Controls.Add(this.btnAddTags);
            this.panel1.Controls.Add(this.btnLimpar);
            this.panel1.Controls.Add(this.btnImprimir);
            this.panel1.Location = new System.Drawing.Point(572, 34);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(83, 247);
            this.panel1.TabIndex = 5;
            // 
            // btnAddTags
            // 
            this.btnAddTags.Location = new System.Drawing.Point(3, 3);
            this.btnAddTags.Name = "btnAddTags";
            this.btnAddTags.Size = new System.Drawing.Size(75, 23);
            this.btnAddTags.TabIndex = 3;
            this.btnAddTags.Text = "Add. Tags";
            this.btnAddTags.UseVisualStyleBackColor = true;
            this.btnAddTags.Click += new System.EventHandler(this.btnAddTags_Click);
            // 
            // btnLimpar
            // 
            this.btnLimpar.Image = global::ACBrLibPosPrinter.Demo.Properties.Resources.limpar;
            this.btnLimpar.Location = new System.Drawing.Point(3, 176);
            this.btnLimpar.Name = "btnLimpar";
            this.btnLimpar.Size = new System.Drawing.Size(75, 30);
            this.btnLimpar.TabIndex = 2;
            this.btnLimpar.Text = "Limpar";
            this.btnLimpar.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
            this.btnLimpar.UseVisualStyleBackColor = true;
            this.btnLimpar.Click += new System.EventHandler(this.btnLimpar_Click);
            // 
            // btnImprimir
            // 
            this.btnImprimir.Image = global::ACBrLibPosPrinter.Demo.Properties.Resources.imprimir;
            this.btnImprimir.Location = new System.Drawing.Point(3, 212);
            this.btnImprimir.Name = "btnImprimir";
            this.btnImprimir.Size = new System.Drawing.Size(75, 30);
            this.btnImprimir.TabIndex = 1;
            this.btnImprimir.Text = "Imprimir";
            this.btnImprimir.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
            this.btnImprimir.UseVisualStyleBackColor = true;
            this.btnImprimir.Click += new System.EventHandler(this.btnImprimir_Click);
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.txtImpressao);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(319, 243);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Texto a Imprimir";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // txtImpressao
            // 
            this.txtImpressao.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txtImpressao.Location = new System.Drawing.Point(3, 3);
            this.txtImpressao.Multiline = true;
            this.txtImpressao.Name = "txtImpressao";
            this.txtImpressao.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtImpressao.Size = new System.Drawing.Size(313, 237);
            this.txtImpressao.TabIndex = 0;
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Location = new System.Drawing.Point(243, 12);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(327, 269);
            this.tabControl1.TabIndex = 0;
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(667, 291);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.tabControl1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibPosPrinter - Teste C#";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudLinhasPular)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudBuffer)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudEspacos)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudColunas)).EndInit();
            this.panel1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabControl1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button btnAtivar;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox cbbModelo;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox cbbPortas;
        private System.Windows.Forms.CheckBox cbxIgnorarTags;
        private System.Windows.Forms.CheckBox cbxTraduzirTags;
        private System.Windows.Forms.CheckBox cbxCortarPapel;
        private System.Windows.Forms.CheckBox cbxControlePorta;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.NumericUpDown nudLinhasPular;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.NumericUpDown nudBuffer;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.NumericUpDown nudEspacos;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.NumericUpDown nudColunas;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox txtArqLog;
        private System.Windows.Forms.Button btnArqLog;
        private System.Windows.Forms.ComboBox cbbPaginaCodigo;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button btnLimpar;
        private System.Windows.Forms.Button btnImprimir;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TextBox txtImpressao;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.Button btnAddTags;
    }
}

