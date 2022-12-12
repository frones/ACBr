namespace ACBrLibNCM.Demo
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
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.txtValidarNCM = new System.Windows.Forms.TextBox();
            this.btnValidarNCM = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.txtFiltrarPorDescricao = new System.Windows.Forms.TextBox();
            this.cmbFiltroPorDescricao = new System.Windows.Forms.ComboBox();
            this.btnFiltrarPorDescricao = new System.Windows.Forms.Button();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.btnFiltrarPorCodigo = new System.Windows.Forms.Button();
            this.txtFiltrarPorCodigo = new System.Windows.Forms.TextBox();
            this.btnBaixarLista = new System.Windows.Forms.Button();
            this.btnObterNCMs = new System.Windows.Forms.Button();
            this.groupBox2.SuspendLayout();
            this.tabControl3.SuspendLayout();
            this.tabPage5.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.rtbRespostas);
            this.groupBox2.Location = new System.Drawing.Point(206, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(515, 376);
            this.groupBox2.TabIndex = 20;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Respostas";
            // 
            // rtbRespostas
            // 
            this.rtbRespostas.Dock = System.Windows.Forms.DockStyle.Fill;
            this.rtbRespostas.Location = new System.Drawing.Point(3, 16);
            this.rtbRespostas.Name = "rtbRespostas";
            this.rtbRespostas.Size = new System.Drawing.Size(509, 357);
            this.rtbRespostas.TabIndex = 3;
            this.rtbRespostas.Text = "";
            // 
            // tabControl3
            // 
            this.tabControl3.Controls.Add(this.tabPage5);
            this.tabControl3.Location = new System.Drawing.Point(12, 12);
            this.tabControl3.Name = "tabControl3";
            this.tabControl3.SelectedIndex = 0;
            this.tabControl3.Size = new System.Drawing.Size(191, 380);
            this.tabControl3.TabIndex = 25;
            // 
            // tabPage5
            // 
            this.tabPage5.Controls.Add(this.groupBox4);
            this.tabPage5.Controls.Add(this.groupBox1);
            this.tabPage5.Controls.Add(this.groupBox3);
            this.tabPage5.Controls.Add(this.btnBaixarLista);
            this.tabPage5.Controls.Add(this.btnObterNCMs);
            this.tabPage5.Location = new System.Drawing.Point(4, 22);
            this.tabPage5.Name = "tabPage5";
            this.tabPage5.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage5.Size = new System.Drawing.Size(183, 354);
            this.tabPage5.TabIndex = 0;
            this.tabPage5.Text = "Comandos";
            this.tabPage5.UseVisualStyleBackColor = true;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.txtValidarNCM);
            this.groupBox4.Controls.Add(this.btnValidarNCM);
            this.groupBox4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox4.Location = new System.Drawing.Point(7, 269);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(171, 78);
            this.groupBox4.TabIndex = 12;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Validar NCM";
            // 
            // txtValidarNCM
            // 
            this.txtValidarNCM.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtValidarNCM.Location = new System.Drawing.Point(4, 19);
            this.txtValidarNCM.Name = "txtValidarNCM";
            this.txtValidarNCM.Size = new System.Drawing.Size(162, 20);
            this.txtValidarNCM.TabIndex = 31;
            // 
            // btnValidarNCM
            // 
            this.btnValidarNCM.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnValidarNCM.Location = new System.Drawing.Point(4, 44);
            this.btnValidarNCM.Name = "btnValidarNCM";
            this.btnValidarNCM.Size = new System.Drawing.Size(162, 23);
            this.btnValidarNCM.TabIndex = 30;
            this.btnValidarNCM.Text = "Validar";
            this.btnValidarNCM.UseVisualStyleBackColor = true;
            this.btnValidarNCM.Click += new System.EventHandler(this.btnValidarNCM_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.txtFiltrarPorDescricao);
            this.groupBox1.Controls.Add(this.cmbFiltroPorDescricao);
            this.groupBox1.Controls.Add(this.btnFiltrarPorDescricao);
            this.groupBox1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox1.Location = new System.Drawing.Point(7, 158);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(171, 105);
            this.groupBox1.TabIndex = 11;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Filtrar Por Descrição";
            // 
            // txtFiltrarPorDescricao
            // 
            this.txtFiltrarPorDescricao.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFiltrarPorDescricao.Location = new System.Drawing.Point(3, 46);
            this.txtFiltrarPorDescricao.Name = "txtFiltrarPorDescricao";
            this.txtFiltrarPorDescricao.Size = new System.Drawing.Size(162, 20);
            this.txtFiltrarPorDescricao.TabIndex = 32;
            // 
            // cmbFiltroPorDescricao
            // 
            this.cmbFiltroPorDescricao.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbFiltroPorDescricao.FormattingEnabled = true;
            this.cmbFiltroPorDescricao.Items.AddRange(new object[] {
            "Inicia Com",
            "Contém",
            "Finaliza Com"});
            this.cmbFiltroPorDescricao.Location = new System.Drawing.Point(6, 19);
            this.cmbFiltroPorDescricao.Name = "cmbFiltroPorDescricao";
            this.cmbFiltroPorDescricao.Size = new System.Drawing.Size(159, 21);
            this.cmbFiltroPorDescricao.TabIndex = 31;
            // 
            // btnFiltrarPorDescricao
            // 
            this.btnFiltrarPorDescricao.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFiltrarPorDescricao.Location = new System.Drawing.Point(3, 72);
            this.btnFiltrarPorDescricao.Name = "btnFiltrarPorDescricao";
            this.btnFiltrarPorDescricao.Size = new System.Drawing.Size(162, 23);
            this.btnFiltrarPorDescricao.TabIndex = 30;
            this.btnFiltrarPorDescricao.Text = "Filtrar";
            this.btnFiltrarPorDescricao.UseVisualStyleBackColor = true;
            this.btnFiltrarPorDescricao.Click += new System.EventHandler(this.btnFiltrarPorDescricao_Click);
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.btnFiltrarPorCodigo);
            this.groupBox3.Controls.Add(this.txtFiltrarPorCodigo);
            this.groupBox3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox3.Location = new System.Drawing.Point(6, 73);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(171, 79);
            this.groupBox3.TabIndex = 10;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Filtrar Por Código";
            // 
            // btnFiltrarPorCodigo
            // 
            this.btnFiltrarPorCodigo.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFiltrarPorCodigo.Location = new System.Drawing.Point(6, 45);
            this.btnFiltrarPorCodigo.Name = "btnFiltrarPorCodigo";
            this.btnFiltrarPorCodigo.Size = new System.Drawing.Size(162, 23);
            this.btnFiltrarPorCodigo.TabIndex = 29;
            this.btnFiltrarPorCodigo.Text = "Filtrar";
            this.btnFiltrarPorCodigo.UseVisualStyleBackColor = true;
            this.btnFiltrarPorCodigo.Click += new System.EventHandler(this.btnFiltrarPorCodigo_Click);
            // 
            // txtFiltrarPorCodigo
            // 
            this.txtFiltrarPorCodigo.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFiltrarPorCodigo.Location = new System.Drawing.Point(6, 19);
            this.txtFiltrarPorCodigo.Name = "txtFiltrarPorCodigo";
            this.txtFiltrarPorCodigo.Size = new System.Drawing.Size(162, 20);
            this.txtFiltrarPorCodigo.TabIndex = 28;
            // 
            // btnBaixarLista
            // 
            this.btnBaixarLista.Location = new System.Drawing.Point(21, 44);
            this.btnBaixarLista.Name = "btnBaixarLista";
            this.btnBaixarLista.Size = new System.Drawing.Size(142, 23);
            this.btnBaixarLista.TabIndex = 1;
            this.btnBaixarLista.Text = "Baixar Lista NCMs";
            this.btnBaixarLista.UseVisualStyleBackColor = true;
            this.btnBaixarLista.Click += new System.EventHandler(this.btnBaixarLista_Click);
            // 
            // btnObterNCMs
            // 
            this.btnObterNCMs.Location = new System.Drawing.Point(21, 15);
            this.btnObterNCMs.Name = "btnObterNCMs";
            this.btnObterNCMs.Size = new System.Drawing.Size(142, 23);
            this.btnObterNCMs.TabIndex = 0;
            this.btnObterNCMs.Text = "Obter NCMs";
            this.btnObterNCMs.UseVisualStyleBackColor = true;
            this.btnObterNCMs.Click += new System.EventHandler(this.btnObterNCMs_Click);
            // 
            // FrmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(733, 398);
            this.Controls.Add(this.tabControl3);
            this.Controls.Add(this.groupBox2);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FrmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ACBrLibNCM Demo";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FrmMain_FormClosing);
            this.Shown += new System.EventHandler(this.FrmMain_Shown);
            this.groupBox2.ResumeLayout(false);
            this.tabControl3.ResumeLayout(false);
            this.tabPage5.ResumeLayout(false);
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RichTextBox rtbRespostas;
        private System.Windows.Forms.TabControl tabControl3;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.Button btnObterNCMs;
        private System.Windows.Forms.Button btnBaixarLista;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.TextBox txtFiltrarPorCodigo;
        private System.Windows.Forms.Button btnFiltrarPorCodigo;
        private System.Windows.Forms.Button btnValidarNCM;
        private System.Windows.Forms.Button btnFiltrarPorDescricao;
        private System.Windows.Forms.TextBox txtValidarNCM;
        private System.Windows.Forms.ComboBox cmbFiltroPorDescricao;
        private System.Windows.Forms.TextBox txtFiltrarPorDescricao;
    }
}

