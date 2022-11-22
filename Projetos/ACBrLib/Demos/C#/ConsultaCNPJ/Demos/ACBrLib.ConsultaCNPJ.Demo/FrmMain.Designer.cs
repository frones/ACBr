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
            this.btnConsultarCaptcha = new System.Windows.Forms.Button();
            this.btnConsultarCNPJ = new System.Windows.Forms.Button();
            this.groupBox2.SuspendLayout();
            this.tabControl3.SuspendLayout();
            this.tabPage5.SuspendLayout();
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
            this.tabControl3.Location = new System.Drawing.Point(12, 12);
            this.tabControl3.Name = "tabControl3";
            this.tabControl3.SelectedIndex = 0;
            this.tabControl3.Size = new System.Drawing.Size(307, 267);
            this.tabControl3.TabIndex = 25;
            // 
            // tabPage5
            // 
            this.tabPage5.Controls.Add(this.btnConsultarCaptcha);
            this.tabPage5.Controls.Add(this.btnConsultarCNPJ);
            this.tabPage5.Location = new System.Drawing.Point(4, 22);
            this.tabPage5.Name = "tabPage5";
            this.tabPage5.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage5.Size = new System.Drawing.Size(299, 241);
            this.tabPage5.TabIndex = 0;
            this.tabPage5.Text = "Consultas";
            this.tabPage5.UseVisualStyleBackColor = true;
            // 
            // btnConsultarCaptcha
            // 
            this.btnConsultarCaptcha.Location = new System.Drawing.Point(15, 21);
            this.btnConsultarCaptcha.Name = "btnConsultarCaptcha";
            this.btnConsultarCaptcha.Size = new System.Drawing.Size(109, 23);
            this.btnConsultarCaptcha.TabIndex = 4;
            this.btnConsultarCaptcha.Text = "Consultar Captcha";
            this.btnConsultarCaptcha.UseVisualStyleBackColor = true;
            this.btnConsultarCaptcha.Click += new System.EventHandler(this.btnConsultarCaptcha_Click);
            // 
            // btnConsultarCNPJ
            // 
            this.btnConsultarCNPJ.Location = new System.Drawing.Point(159, 21);
            this.btnConsultarCNPJ.Name = "btnConsultarCNPJ";
            this.btnConsultarCNPJ.Size = new System.Drawing.Size(109, 23);
            this.btnConsultarCNPJ.TabIndex = 0;
            this.btnConsultarCNPJ.Text = "Consultar CNPJ";
            this.btnConsultarCNPJ.UseVisualStyleBackColor = true;
            this.btnConsultarCNPJ.Click += new System.EventHandler(this.btnConsultarCNPJ_Click);
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
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RichTextBox rtbRespostas;
        private System.Windows.Forms.TabControl tabControl3;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.Button btnConsultarCNPJ;
        private System.Windows.Forms.Button btnConsultarCaptcha;
    }
}

