using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.ConsultaCNPJ;
using System.Linq;
using static ACBrLib.ConsultaCNPJ.ACBrConsultaCNPJ;

namespace ACBrLibConsultaCNPJ.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrConsultaCNPJ ACBrCNPJ;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrCNPJ = new ACBrConsultaCNPJ();
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando...");

            try
            {
                // Altera as config de log
                ACBrCNPJ.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrCNPJ.Config.Principal.LogPath = logPath;
                ACBrCNPJ.ConfigGravar();

                LoadConfig();

            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll
            ACBrCNPJ.Dispose();
        }

        #endregion EventHandlers

        private void btnConsultarCNPJ_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.Clear();

                if (!string.IsNullOrEmpty(edtCNPJ.Text))
                {
                    var ret = ACBrCNPJ.Consultar(edtCNPJ.Text);
                    rtbRespostas.AppendText(ret);
                }
                else
                {
                    MessageBox.Show("Informe o CNPJ !");
                }


            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCaptcha_Click(object sender, EventArgs e)
        {
            try
            {

                var captchaPath = Path.Combine(Application.StartupPath, "Captcha");
                if (!Directory.Exists(captchaPath))
                    Directory.CreateDirectory(captchaPath);

                ACBrCNPJ.ConsultarCaptcha(captchaPath);

            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion Methods

        private void btnSalvarINI_Click(object sender, EventArgs e)
        {
            try
            {
                SalvarConfig();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {

                ACBrCNPJ.Config.Provedor = cmbServico.SelectedIndex;
                ACBrCNPJ.Config.Usuario  = txtUsuario.Text;
                ACBrCNPJ.Config.Senha = txtSenha.Text;

                ACBrCNPJ.Config.Proxy.Servidor = textHost.Text;
                ACBrCNPJ.Config.Proxy.Porta = textPort.Text;
                ACBrCNPJ.Config.Proxy.Usuario = textUser.Text;
                ACBrCNPJ.Config.Proxy.Senha = textPassword.Text;

                ACBrCNPJ.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnLerINI_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            ACBrCNPJ.ConfigLer(file);

            cmbServico.SelectedIndex = ACBrCNPJ.Config.Provedor;
            txtUsuario.Text = ACBrCNPJ.Config.Usuario;
            txtSenha.Text = ACBrCNPJ.Config.Senha;

            textHost.Text = ACBrCNPJ.Config.Proxy.Servidor;
            textPort.Text = ACBrCNPJ.Config.Proxy.Porta;
            textUser.Text = ACBrCNPJ.Config.Proxy.Usuario;
            textPassword.Text = ACBrCNPJ.Config.Proxy.Senha;


        }
    }
}