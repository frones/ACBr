using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Sedex;

namespace ACBrLibSedex.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrSedex ACBrSedex;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrSedex = new ACBrSedex();
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
                ACBrSedex.Config.Principal.LogNivel = NivelLog.logCompleto;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrSedex.Config.Principal.LogPath = logPath;
                ACBrSedex.ConfigGravar();

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
            ACBrSedex.Dispose();
        }

        #endregion EventHandlers

        #endregion Methods

        private void btnSalvarConfiguracoes_Click(object sender, EventArgs e)
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

                ACBrSedex.Config.CodContrato = txtCodContrato.Text;
                ACBrSedex.Config.Senha = txtSenha.Text;

                ACBrSedex.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            ACBrSedex.ConfigLer(file);

            txtCodContrato.Text = ACBrSedex.Config.CodContrato;
            txtSenha.Text = ACBrSedex.Config.Senha;

        }

        private void btnSalvarConfiguracoes_Click_1(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void btnCarregarConfiguracoes_Click_1(object sender, EventArgs e)
        {
            LoadConfig();
        }

        private void btnRastrear_Click(object sender, EventArgs e)
        {
            if (txtCodRastreio.Text == "")
            {
                MessageBox.Show("Informe o Código de Rastreio");
                return;
            }

            try
            {
                txtRetorno.AppendText(ACBrSedex.Rastrear(txtCodRastreio.Text));
            } 
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultar_Click(object sender, EventArgs e)
        {
            try
            {
                txtRetorno.AppendText(ACBrSedex.Consultar());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}