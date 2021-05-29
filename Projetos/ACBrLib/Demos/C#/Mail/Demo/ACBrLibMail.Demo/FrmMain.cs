using ACBrLib.Core;
using ACBrLib.Mail;
using System;
using System.Windows.Forms;

namespace ACBrLibMail.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrMail acBrMail;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            acBrMail = new ACBrMail();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void btnSalvar_Click(object sender, EventArgs e)
        {
            try
            {
                acBrMail.Config.Nome = txtNome.Text;
                acBrMail.Config.Conta = txtEmail.Text;
                acBrMail.Config.Usuario = txtUsuario.Text;
                acBrMail.Config.Senha = txtSenha.Text;
                acBrMail.Config.Servidor = txtHost.Text;
                acBrMail.Config.Porta = nudPorta.Text;
                acBrMail.Config.SSL = ckbSSL.Checked;
                acBrMail.Config.TLS = ckbTLS.Checked;

                acBrMail.ConfigGravar();
                MessageBox.Show(@"Configuração salva com sucesso!", @"ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show($@"Erro ao gravar dados no INI: {ex.Message}", @"ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviar_Click(object sender, EventArgs e)
        {
            try
            {
                acBrMail.Clear();
                acBrMail.AddAddress(txtDestinatario.Text, txtDestinatario.Text);
                acBrMail.SetSubject(txtAssunto.Text);
                acBrMail.AddBody(txtBody.Text);
                acBrMail.AddAltBody(txtAltBody.Text);

                foreach (string anexo in lstAnexos.Items)
                {
                    acBrMail.AddAttachment(@anexo, "Teste de Anexo", ACBrLib.Core.Mail.MailAttachmentDisposition.Inline);
                }

                acBrMail.Send();

                MessageBox.Show(@"Email enviado com sucesso!", @"ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show($@"Erro ao enviar email: {ex.Message}", @"ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void LoadConfig()
        {
            acBrMail.ConfigLer();

            txtNome.Text = acBrMail.Config.Nome;
            txtEmail.Text = acBrMail.Config.Conta;
            txtUsuario.Text = acBrMail.Config.Usuario;
            txtSenha.Text = acBrMail.Config.Senha;
            txtHost.Text = acBrMail.Config.Servidor;
            nudPorta.Text = acBrMail.Config.Porta;
            ckbSSL.Checked = acBrMail.Config.SSL;
            ckbTLS.Checked = acBrMail.Config.TLS;
        }

        #endregion Methods

        private void button1_Click(object sender, EventArgs e)
        {
            if (openFileDialog1.ShowDialog() != DialogResult.OK) return;
            foreach (var arquivo in openFileDialog1.FileNames)
                lstAnexos.Items.Add(arquivo);
        }

        private void lstAnexos_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
                e.Effect = DragDropEffects.All;
        }

        private void lstAnexos_DragDrop(object sender, DragEventArgs e)
        {
            var files = (string[])e.Data.GetData(DataFormats.FileDrop);
            foreach (var f in files)
                lstAnexos.Items.Add(f);
        }

        private void FrmMain_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.All;
        }

        private void lstAnexos_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode != Keys.Delete) return;

            if (lstAnexos.SelectedIndex >= 0)
                lstAnexos.Items.Remove(lstAnexos.Items[lstAnexos.SelectedIndex]);
        }
    }
}