using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.IBGE;
using ACBrLib.Core.IBGE;

namespace ACBrLibIBGE.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrIBGE ACBrIBGE;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrIBGE = new ACBrIBGE();
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
                ACBrIBGE.Config.Principal.LogNivel = NivelLog.logCompleto;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrIBGE.Config.Principal.LogPath = logPath;
                ACBrIBGE.ConfigGravar();

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
            ACBrIBGE.Dispose();
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
                
                ACBrIBGE.Config.IgnorarCaixaEAcentos = chkBoxIgnorarCaixaseAcentos.Checked;

                ACBrIBGE.ConfigGravar();

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
            ACBrIBGE.ConfigLer(file);

            chkBoxIgnorarCaixaseAcentos.Checked = ACBrIBGE.Config.IgnorarCaixaEAcentos;

        }

        private void btnBuscarPorCodigo_Click(object sender, EventArgs e)
        {

            if (txtCodMunicipio.Text == "")
            {
                MessageBox.Show("Informe o Codigo do Municipio");
                return;
            }

            try
            {
                txtRetorno.AppendText(ACBrIBGE.buscarPorCodigo(Convert.ToInt32(txtCodMunicipio.Text)));
            } 
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnBuscarPorNome_Click(object sender, EventArgs e)
        {

            if (txtCidade.Text == "")
            {
                MessageBox.Show("Informe a Cidade");
                return;
            }

            try
            {
                txtRetorno.AppendText(ACBrIBGE.buscarPorNome(txtCidade.Text, txtUF.Text, false));
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }            
        }

        private void btnSalvarConfiguracoes_Click_1(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void btnCarregarConfiguracoes_Click_1(object sender, EventArgs e)
        {
            LoadConfig();
        }
    }
}