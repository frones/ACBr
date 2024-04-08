using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.eSocial;
using ACBrLib.GTIN;
using System.Linq;

namespace ACBrLibGTIN.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrGTIN ACBrGTIN;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrGTIN = new ACBrGTIN();
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
                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                // Altera as config de log
                ACBrGTIN.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrGTIN.Config.Principal.LogPath = logPath;
                ACBrGTIN.ConfigGravar();

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
            ACBrGTIN.Dispose();
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
                //Config Geral
                ACBrGTIN.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBrGTIN.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBrGTIN.Config.SalvarGer = ckbSalvarArqeResp.Checked;
                ACBrGTIN.Config.PathSalvar = txtLogs.Text;
                ACBrGTIN.Config.PathSchemas = txtSchemaPath.Text;

                //Config WebService
                ACBrGTIN.Config.DFe.UF = cmbUfDestino.Text;
                ACBrGTIN.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBrGTIN.Config.Timeout = (int)nudTimeOut.Value;
                ACBrGTIN.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBrGTIN.Config.Visualizar = ckbVisualizar.Checked;
                ACBrGTIN.Config.SalvarWS = ckbSalvarSOAP.Checked;

                //Proxy
                ACBrGTIN.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBrGTIN.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrGTIN.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBrGTIN.Config.Proxy.Senha = txtProxySenha.Text;

                //Certificado
                ACBrGTIN.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBrGTIN.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBrGTIN.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBrGTIN.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBrGTIN.Config.DFe.Senha = txtCertPassword.Text;
                ACBrGTIN.Config.DFe.NumeroSerie = txtCertNumero.Text;
                ACBrGTIN.Config.DFe.DadosPFX = txtDadosPFX.Text;

                //Arquivos
                ACBrGTIN.Config.SalvarArq = ckbSalvarArqs.Checked;
                ACBrGTIN.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                ACBrGTIN.Config.PathGTIN = txtArqGTIN.Text;

                ACBrGTIN.ConfigGravar();

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
            ACBrGTIN.ConfigLer(file);

            //Config Geral

            ckbExibirErroSchema.Checked = ACBrGTIN.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBrGTIN.Config.FormatoAlerta;
            ckbSalvarArqeResp.Checked = ACBrGTIN.Config.SalvarGer;
            txtLogs.Text = ACBrGTIN.Config.PathSalvar;
            txtSchemaPath.Text = ACBrGTIN.Config.PathSchemas;

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrGTIN.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBrGTIN.Config.SSLType);
            nudTimeOut.Value = ACBrGTIN.Config.Timeout;

            var ambiente = ACBrGTIN.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrGTIN.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBrGTIN.Config.SalvarWS;
            
            //Proxy
            txtProxyServidor.Text = ACBrGTIN.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrGTIN.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrGTIN.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrGTIN.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrGTIN.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrGTIN.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrGTIN.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrGTIN.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrGTIN.Config.DFe.Senha;
            txtCertNumero.Text = ACBrGTIN.Config.DFe.NumeroSerie;
            txtDadosPFX.Text = ACBrGTIN.Config.DFe.DadosPFX;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrGTIN.Config.SalvarArq;
            ckbAdicionaLiteral.Checked = ACBrGTIN.Config.AdicionarLiteral;
            txtArqGTIN.Text = ACBrGTIN.Config.PathGTIN;

        }

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void btnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = ACBrGTIN.OpenSSLInfo();
            rtbRespostas.AppendText(ret);
        }

        private void btnArqGTIN_Click(object sender, EventArgs e)
        {
            txtArqGTIN.Text = Helpers.SelectFolder();
        }

        private void btnConsultarGTIN_Click(object sender, EventArgs e)
        {
            try
            {
                var codGTIN = "";
                if (InputBox.Show("Consultar GTIN", "Código GTIN:", ref codGTIN) != DialogResult.OK) return;

                var ret = ACBrGTIN.Consultar(codGTIN);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}