using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Reinf;
using ACBrLib.Reinf;
using System.Linq;

namespace ACBrLibReinf.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrReinf ACBrReinf;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrReinf = new ACBrReinf();
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
                cmbTipoContribuinte.EnumDataSource(TipoContribuinte.tcPessoaJuridica);
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbVersaoDF.SelectedIndex = 5;

                // Altera as config de log
                ACBrReinf.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrReinf.Config.Principal.LogPath = logPath;
                ACBrReinf.ConfigGravar();

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
            ACBrReinf.Dispose();
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
                ACBrReinf.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBrReinf.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBrReinf.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                ACBrReinf.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                ACBrReinf.Config.SalvarWS = ckbSalvar.Checked;
                ACBrReinf.Config.PathSalvar = txtLogs.Text;
                ACBrReinf.Config.PathSchemas = txtSchemaPath.Text;
                ACBrReinf.Config.IdContribuinte = txtContribuinte.Text;
                ACBrReinf.Config.IdTransmissor = txtTransmissor.Text;
                ACBrReinf.Config.TipoContribuinte = cmbTipoContribuinte.GetSelectedValue<TipoContribuinte>();
                ACBrReinf.Config.VersaoDF = cmbVersaoDF.SelectedIndex.ToString();

                //Config WebService
                ACBrReinf.Config.DFe.UF = cmbUfDestino.Text;

                ACBrReinf.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBrReinf.Config.Timeout = (int)nudTimeOut.Value;
                ACBrReinf.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBrReinf.Config.Visualizar = ckbVisualizar.Checked;
                ACBrReinf.Config.SalvarWS = ckbSalvarSOAP.Checked;
                ACBrReinf.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
                ACBrReinf.Config.AguardarConsultaRet = (int)nudAguardar.Value;
                ACBrReinf.Config.Tentativas = (int)nudTentativas.Value;
                ACBrReinf.Config.IntervaloTentativas = (int)nudIntervalos.Value;

                //Proxy
                ACBrReinf.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBrReinf.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrReinf.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBrReinf.Config.Proxy.Senha = txtProxySenha.Text;

                //Certificado
                ACBrReinf.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBrReinf.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBrReinf.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBrReinf.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBrReinf.Config.DFe.Senha = txtCertPassword.Text;
                ACBrReinf.Config.DFe.NumeroSerie = txtCertNumero.Text;
                ACBrReinf.Config.DFe.DadosPFX = txtDadosPFX.Text;

                //Arquivos
                ACBrReinf.Config.SalvarGer = ckbSalvarArqs.Checked;
                ACBrReinf.Config.SepararPorMes = ckbPastaMensal.Checked;
                ACBrReinf.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                ACBrReinf.Config.EmissaoPathReinf = ckbEmissaoPathReinf.Checked;
                ACBrReinf.Config.SalvarArq = ckbSalvaPathEvento.Checked;
                ACBrReinf.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
                ACBrReinf.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
                ACBrReinf.Config.PathReinf = txtArqReinf.Text;

                ACBrReinf.ConfigGravar();

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
            ACBrReinf.ConfigLer(file);

            //Config Geral

            ckbExibirErroSchema.Checked = ACBrReinf.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBrReinf.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(ACBrReinf.Config.FormaEmissao);
            ckbRetirarAcentos.Checked = ACBrReinf.Config.RetirarAcentos;
            ckbSalvar.Checked = ACBrReinf.Config.SalvarWS;
            txtLogs.Text = ACBrReinf.Config.PathSalvar;
            txtSchemaPath.Text = ACBrReinf.Config.PathSchemas;
            txtContribuinte.Text = ACBrReinf.Config.IdContribuinte;
            txtTransmissor.Text = ACBrReinf.Config.IdTransmissor;
            cmbTipoContribuinte.SetSelectedValue(ACBrReinf.Config.TipoContribuinte);
            cmbVersaoDF.SelectedIndex = Int32.Parse(ACBrReinf.Config.VersaoDF);

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrReinf.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBrReinf.Config.SSLType);
            nudTimeOut.Value = ACBrReinf.Config.Timeout;

            var ambiente = ACBrReinf.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrReinf.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBrReinf.Config.SalvarWS;
            ckbAjustarAut.Checked = ACBrReinf.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = ACBrReinf.Config.AguardarConsultaRet;
            nudTentativas.Value = ACBrReinf.Config.Tentativas;
            nudIntervalos.Value = ACBrReinf.Config.IntervaloTentativas;
            
            //Proxy
            txtProxyServidor.Text = ACBrReinf.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrReinf.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrReinf.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrReinf.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrReinf.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrReinf.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrReinf.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrReinf.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrReinf.Config.DFe.Senha;
            txtCertNumero.Text = ACBrReinf.Config.DFe.NumeroSerie;
            txtDadosPFX.Text = ACBrReinf.Config.DFe.DadosPFX;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrReinf.Config.SalvarGer;
            ckbPastaMensal.Checked = ACBrReinf.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = ACBrReinf.Config.AdicionarLiteral;
            ckbEmissaoPathReinf.Checked = ACBrReinf.Config.EmissaoPathReinf;
            ckbSalvaPathEvento.Checked = ACBrReinf.Config.SalvarArq;
            ckbSepararPorCNPJ.Checked = ACBrReinf.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = ACBrReinf.Config.SepararPorModelo;
            txtArqReinf.Text = ACBrReinf.Config.PathReinf;

        }

        private void btnCriarEventoReinf_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrReinf.CriarEventoReinf(arquivoIni);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarReinf_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrReinf.EnviarReinf();
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarReinf_Click(object sender, EventArgs e)
        {
            try
            {
                string protocolo = "2." + DateTime.Now.ToString("yyyyMM") + ".0000000";
                if (InputBox.Show("Consultar Reinf", "Consultar", ref protocolo) != DialogResult.OK) return;

                var ret = ACBrReinf.ConsultarReinf(protocolo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            try
            {
                //string protocolo = "2." + DateTime.Now.ToString("yyyyMM") + ".0000000";
                //if (InputBox.Show("Consultar Reinf", "Consultar", ref protocolo) != DialogResult.OK) return;

                string ePerApur = DateTime.Now.ToString("yyyy-MM");
                if (InputBox.Show("Consultar Recibo", "PerApur", ref ePerApur) != DialogResult.OK) return;

                int aTipoEvento = 1000;
                if (InputBox.Show("Consultar Recibo", "TipoEvento", ref aTipoEvento) != DialogResult.OK) return;

                string eNrInscEstab = txtContribuinte.Text;
                if (InputBox.Show("Consultar Recibo", "NrInscEstab", ref eNrInscEstab) != DialogResult.OK) return;

                string eCnpjPrestador = "";
                if (InputBox.Show("Consultar Recibo", "CnpjPrestador", ref eCnpjPrestador) != DialogResult.OK) return;

                string eNrInscTomador = "";
                if (InputBox.Show("Consultar Recibo", "NrInscTomador", ref eNrInscTomador) != DialogResult.OK) return;

                string eDtApur = "";
                if (InputBox.Show("Consultar Recibo", "DtApur", ref eDtApur) != DialogResult.OK) return;

                string eCpfCnpjBenef = "";
                if (InputBox.Show("Consultar Recibo", "CpfCnpjBenef", ref eCpfCnpjBenef) != DialogResult.OK) return;

                string eCnpjFonte = "";
                if (InputBox.Show("Consultar Recibo", "CnpjFonte", ref eCnpjFonte) != DialogResult.OK) return;

                var ret = ACBrReinf.ConsultarReciboReinf(ePerApur, aTipoEvento, eNrInscEstab, eCnpjPrestador,
                                                         eNrInscTomador, eDtApur, eCpfCnpjBenef, eCnpjFonte);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCriarEnviarReinf_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                var ret = ACBrReinf.CriarEnviarReinf(arquivoIni);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparReinf_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibReinf", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrReinf.LimparReinf();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarXMLEventoReinf_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoxml = Helpers.OpenFile("Arquivo Xml Reinf (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoxml)) return;

                ACBrReinf.CarregarXMLEventoReinf(arquivoxml);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetIDContribuinte_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrReinf.SetIDContribuinte(txtContribuinte.Text);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetIDTransmissor_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrReinf.SetIDTransmissor(txtTransmissor.Text);
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetTipoContribuinte_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrReinf.TipoContribuinte(cmbTipoContribuinte.SelectedIndex);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetVersaoDF_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrReinf.SetVersao(cmbVersaoDF.SelectedIndex.ToString());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            var ret = ACBrReinf.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
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

        private void btnArqReinf_Click(object sender, EventArgs e)
        {
            txtArqReinf.Text = Helpers.SelectFolder();
        }
    }
}