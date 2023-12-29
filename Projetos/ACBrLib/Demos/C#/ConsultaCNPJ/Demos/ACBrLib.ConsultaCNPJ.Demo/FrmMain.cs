using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.ConsultaCNPJ;
using System.Linq;
using System.Drawing;

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

                var eCNPJ = "";
                if (InputBox.Show("Consultar CNPJ", "Informe o CNPJ:", ref eCNPJ) != DialogResult.OK) return;

                var ret = ACBrCNPJ.Consultar(eCNPJ, (int)cmbServico.SelectedIndex );
                rtbRespostas.AppendText(ret);

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
    }
}