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

            loadConfig();
        }

        #endregion Constructors

        #region Methods

        private void btnSalvar_Click(object sender, EventArgs e)
        {

            try
            {
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "Porta", (int)nudPorta.Value);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
                acBrMail.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);

                acBrMail.ConfigGravar("");
                MessageBox.Show("Configuração salva com sucesso!", "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Erro ao gravar dados no INI: " + ex.Message.ToString(), "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
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

                foreach( String anexo in lstAnexos.Items)
                {
                    acBrMail.AddAttachment(@anexo, "Teste de Anexo", ACBrLib.Core.Mail.MailAttachmentDisposition.Inline);
                }

                acBrMail.Send();

                MessageBox.Show("Email enviado com sucesso!","ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Erro ao enviar email: " + ex.Message.ToString(), "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }            
        }

        private void loadConfig()
        {
            acBrMail.ConfigLer();

            txtNome.Text = acBrMail.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = acBrMail.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = acBrMail.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = acBrMail.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = acBrMail.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = acBrMail.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = acBrMail.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = acBrMail.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        #endregion Methods
                
        private void button1_Click(object sender, EventArgs e)
        {
            if (this.openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                foreach (String arquivo in openFileDialog1.FileNames)
                {
                    lstAnexos.Items.Add(arquivo);                    
                }
            }
        }

        private void lstAnexos_DragEnter(object sender, DragEventArgs e)
        {
            if ( e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect = DragDropEffects.All;
            }
        }

        private void lstAnexos_DragDrop(object sender, DragEventArgs e)
        {
            string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
            foreach(string f in files)
            {
                lstAnexos.Items.Add(f);
            }
        }

        private void FrmMain_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.All;
        }

        private void lstAnexos_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Delete)
            {
                if ( lstAnexos.SelectedIndex >= 0 )
                {
                    lstAnexos.Items.Remove(lstAnexos.Items[lstAnexos.SelectedIndex]);
                }
            }
        }
    }
}