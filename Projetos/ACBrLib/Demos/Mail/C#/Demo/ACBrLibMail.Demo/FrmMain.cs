using System;
using System.Windows.Forms;

namespace ACBrLibMail.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            var ret = ACBrMail.MAIL_Inicializar("".ToUTF8(), "".ToUTF8());
            ACBrMail.CheckResult(ret);
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            var ret = ACBrMail.MAIL_Finalizar();
            ACBrMail.CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        private void btnSalvar_Click(object sender, EventArgs e)
        {
            var ret = ACBrMail.MAIL_ConfigGravarValor("Email".ToUTF8(), "Nome".ToUTF8(), txtNome.Text.ToUTF8());
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
        }

        private void btnEnviar_Click(object sender, EventArgs e)
        {
            var ret = ACBrMail.MAIL_Clear();
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
        }

        #endregion Methods
    }
}