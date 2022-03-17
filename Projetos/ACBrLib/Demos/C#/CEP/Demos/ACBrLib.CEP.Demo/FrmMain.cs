using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.CEP;
using ACBrLib.Core.CEP;
using System.Linq;

namespace ACBrLibCEP.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrCEP ACBrCEP;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrCEP = new ACBrCEP();
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
                cmbWebService.EnumDataSource(WebService.wsNenhum);

                // Altera as config de log
                ACBrCEP.Config.Principal.LogNivel = NivelLog.logCompleto;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrCEP.Config.Principal.LogPath = logPath;
                ACBrCEP.ConfigGravar();

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
            ACBrCEP.Dispose();
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
                ACBrCEP.Config.WebService = cmbWebService.GetSelectedValue<WebService>();
                ACBrCEP.Config.ChaveAcesso = txtChaveWebService.Text;
                ACBrCEP.Config.Usuario = txtUsuarioWebService.Text;
                ACBrCEP.Config.Senha = txtSenhaWebService.Text;
                ACBrCEP.Config.PesquisarIBGE = chkPesquisarIBGE.Checked;

                ACBrCEP.Config.Proxy.Servidor = txtHostProxy.Text;
                ACBrCEP.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrCEP.Config.Proxy.Usuario = txtUsuarioProxy.Text;
                ACBrCEP.Config.Proxy.Senha = txtSenhaProxy.Text;

                ACBrCEP.ConfigGravar();

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
            ACBrCEP.ConfigLer(file);

            cmbWebService.SetSelectedValue(ACBrCEP.Config.WebService);
            txtChaveWebService.Text = ACBrCEP.Config.ChaveAcesso;
            txtUsuarioWebService.Text = ACBrCEP.Config.Usuario;
            txtSenhaWebService.Text = ACBrCEP.Config.Senha;
            chkPesquisarIBGE.Checked = ACBrCEP.Config.PesquisarIBGE;

            txtHostProxy.Text = ACBrCEP.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrCEP.Config.Proxy.Porta;
            txtUsuarioProxy.Text = ACBrCEP.Config.Proxy.Usuario;
            txtSenhaProxy.Text = ACBrCEP.Config.Proxy.Senha;
        }

        private void btnBuscarPorCEP_Click(object sender, EventArgs e)
        {
            var ret = ACBrCEP.BuscarPorCep(txtCEP.Text);
            txtRetorno.AppendLine(ret.Resposta);
        }

        private void btnBuscarPorLogradouro_Click(object sender, EventArgs e)
        {
            var ret = ACBrCEP.BuscarPorLogradouro(txtCidade.Text, txtTipoLogradouro.Text, txtLogradouro.Text, txtUF.Text, txtBairro.Text);
            txtRetorno.AppendLine(ret.Select(x => x.Resposta).ToArray());

        }
    }
}