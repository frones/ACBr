using System;
using System.Text;
using System.Windows.Forms;

namespace ACBrLibMail.Demo
{
    public partial class FrmMain : Form
    {
        private const int BUFFER_LEN = 256;
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
                int ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Nome".ToUTF8(), txtNome.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Conta".ToUTF8(), txtEmail.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Usuario".ToUTF8(), txtUsuario.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Senha".ToUTF8(), txtSenha.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Servidor".ToUTF8(), txtHost.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Porta".ToUTF8(), nudPorta.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "SSL".ToUTF8(), Convert.ToInt32(ckbSSL.Checked).ToString().ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "TLS".ToUTF8(), Convert.ToInt32(ckbTLS.Checked).ToString().ToUTF8());
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

                ret = ACBrMail.MAIL_AddAddress(txtDestinatario.Text.ToUTF8(), txtDestinatario.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_SetSubject(txtAssunto.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_AddBody(txtBody.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_AddAltBody(txtAltBody.Text.ToUTF8());
                ACBrMail.CheckResult(ret);

                ret = ACBrMail.MAIL_Send();
                ACBrMail.CheckResult(ret);

                MessageBox.Show("Email enviado com sucesso!", "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Erro ao enviar email: " + ex.Message.ToString(), "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void loadConfig()
        {
            try
            {
                int ret = ACBrMail.MAIL_ConfigLer("".ToUTF8());
                ACBrMail.CheckResult(ret);

                var bufferLen = BUFFER_LEN;
                var pValue = new StringBuilder(bufferLen);

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "Nome".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                txtNome.Text = pValue.FromUTF8();

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "Conta".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                txtEmail.Text = pValue.FromUTF8();

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "Usuario".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                txtUsuario.Text = pValue.FromUTF8();

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "Senha".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                txtSenha.Text = pValue.FromUTF8();

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "Servidor".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                txtHost.Text = pValue.FromUTF8();

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "Porta".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                nudPorta.Value = Convert.ToInt32(pValue.FromUTF8());

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "SSL".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                ckbSSL.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

                ret = ACBrMail.MAIL_ConfigLerValor("Email".ToUTF8(), "TLS".ToUTF8(), pValue, ref bufferLen);
                ACBrMail.CheckResult(ret);

                ckbTLS.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));
            }
            catch (Exception ex)
            {
                MessageBox.Show("Erro ao ler dados no INI: " + ex.Message.ToString(), "ACBrMail - Demo", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }


        }

        #endregion Methods
    }
}