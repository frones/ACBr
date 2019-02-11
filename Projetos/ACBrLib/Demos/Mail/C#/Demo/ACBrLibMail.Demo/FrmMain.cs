using System;
using System.Text;
using System.Windows.Forms;

namespace ACBrLibMail.Demo
{
    public partial class FrmMain : Form
    {
        private const int BUFFER_LEN = 256;
        private string chaveEmail;
        
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            int ret = ACBrMail.MAIL_Inicializar("".ToUTF8(), "".ToUTF8());
            ACBrMail.CheckResult(ret);

            loadConfig();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            int ret = ACBrMail.MAIL_Finalizar();
            ACBrMail.CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        private void btnSalvar_Click(object sender, EventArgs e)
        {
            try
            {
                string eSessao = "Email".ToUTF8();
                int ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "Nome".ToUTF8(), txtNome.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "Conta".ToUTF8(), txtEmail.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "Usuario".ToUTF8(), txtUsuario.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "Senha".ToUTF8(), txtSenha.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "Servidor".ToUTF8(), txtHost.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "Porta".ToUTF8(), nudPorta.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "SSL".ToUTF8(), Convert.ToInt32(ckbSSL.Checked).ToString().ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor(eSessao, "TLS".ToUTF8(), Convert.ToInt32(ckbTLS.Checked).ToString().ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravar("".ToUTF8());
                ACBrMail.CheckResult(ret);

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
                int ret = ACBrMail.MAIL_Clear();
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "IsHTML", "1");
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_AddAddress(txtDestinatario.Text.ToUTF8(), txtDestinatario.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_SetSubject(txtAssunto.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_AddBody(txtBody.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_AddAltBody(txtAltBody.Text.ToUTF8());
                ACBrMail.CheckResult(ret);


                foreach (String anexo in lstAnexos.Items)
                {
                    ret = ACBrMail.MAIL_AddAttachment(anexo, anexo, 0);
                    ACBrMail.CheckResult(ret);
                }
                
                ret = ACBrMail.MAIL_Send();
                ACBrMail.CheckResult(ret);
                
                MessageBox.Show("Email enviado com sucesso!", "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Erro ao enviar email: " + ex.Message.ToString(), "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            
        }

        private string configToString(string chave)
        {
            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            int ret;
            
            ret = ACBrMail.MAIL_ConfigLerValor(chaveEmail, chave.ToUTF8(), pValue, ref bufferLen);
            ACBrMail.CheckResult(ret);
            return pValue.FromUTF8();
        } 

        private void loadConfig()
        {
            try
            {
                int ret = ACBrMail.MAIL_ConfigLer("".ToUTF8());
                ACBrMail.CheckResult(ret);
                chaveEmail = "Email".ToUTF8();

                txtNome.Text = configToString("Nome");
                txtEmail.Text = configToString("Conta");
                txtUsuario.Text = configToString("Usuario");
                txtSenha.Text = configToString("Senha");
                txtHost.Text = configToString("Servidor");
                nudPorta.Value = Convert.ToInt32(configToString("Porta"));
                ckbSSL.Checked = Convert.ToBoolean(Convert.ToInt32( configToString("SSL") ));
                ckbTLS.Checked = Convert.ToBoolean(Convert.ToInt32( configToString("TLS") ));
            }
            catch (Exception ex)
            {
                MessageBox.Show("Erro ao ler dados no INI: " + ex.Message.ToString(), "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }


        }

        #endregion Methods

        private void lstAnexos_DragDrop(object sender, DragEventArgs e)
        {
            string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
            foreach (string f in files)
            {
                lstAnexos.Items.Add(f);
            }
        }

        private void lstAnexos_DragEnter(object sender, DragEventArgs e)
        {
            if ( e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect = DragDropEffects.All;
            }
        }

        private void lstAnexos_KeyDown(object sender, KeyEventArgs e)
        {
            if ( e.KeyCode == Keys.Delete )
            {
                if ( lstAnexos.SelectedIndex >= 0 )
                {
                    lstAnexos.Items.Remove(lstAnexos.Items[lstAnexos.SelectedIndex]);
                }
            }
        }

        private void FrmMain_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.All;
        }

        private void openFileDialog1_FileOk(object sender, System.ComponentModel.CancelEventArgs e)
        {

        }

        private void button1_Click(object sender, EventArgs e)
        {
            if ( this.openFileDialog1.ShowDialog() == DialogResult.OK )
            {
                foreach (string arquivo in openFileDialog1.FileNames)
                {
                    lstAnexos.Items.Add(arquivo);
                }
                {

                }
            }
        }
    }
}