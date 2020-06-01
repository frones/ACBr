using System;
using System.Drawing.Printing;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.GNRe;
using ACBrLib.Core.PosPrinter;

namespace ACBrLib.GNRe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrGNRe gnre;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            gnre = new ACBrGNRe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            gnre.Dispose();
            gnre = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.Default.ShowInfo(SplashInfo.Message, "Carregando...");

            try
            {
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbVersaoDF.EnumDataSource(VersaoGNRe.ve200);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);

                // Altera as config de log
                gnre.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                gnre.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
                gnre.ConfigGravar();

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.Default.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "ExibirErroSchema", ckbExibirErroSchema.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "FormatoAlerta", txtFormatoAlerta.Text);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "FormaEmissao",
                    cmbFormaEmissao.GetSelectedValue<TipoEmissao>());
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "VersaoDF", cmbVersaoDF.GetSelectedValue<VersaoGNRe>());
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "RetirarAcentos", ckbRetirarAcentos.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SalvarWS", ckbSalvar.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "PathSalvar", txtLogs.Text);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "PathSchemas", txtSchemaPath.Text);

                //Config Webservice
                gnre.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "Timeout", nudTimeOut.Text);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "Ambiente", rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "Visualizar", ckbVisualizar.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SalvarWS", ckbSalvarSOAP.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "AjustaAguardaConsultaRet", ckbAjustarAut.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "AguardarConsultaRet", (int)nudAguardar.Value);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "Tentativas", (int)nudTentativas.Value);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "IntervaloTentativas", (int)nudIntervalos.Value);
                gnre.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
                gnre.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
                gnre.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
                gnre.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);

                //Config Certificado
                gnre.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
                gnre.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
                gnre.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib", cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
                gnre.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
                gnre.ConfigGravarValor(ACBrSessao.DFe, "DadosPFX", txtDadosPFX.Text);
                gnre.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
                gnre.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);

                //Config Arquivos
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SalvarGer", ckbSalvarArqs.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SepararPorMes", ckbPastaMensal.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "AdicionarLiteral", ckbAdicionaLiteral.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "EmissaoPathGNRe", ckbEmissaoPathGNRe.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SepararPorCNPJ", ckbSepararPorCNPJ.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "SepararPorModelo", ckbSepararPorModelo.Checked);
                gnre.ConfigGravarValor(ACBrSessao.GNRe, "PathGNRe", txtArqGNRe.Text);

                //Config Email
                gnre.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
                gnre.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
                gnre.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
                gnre.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
                gnre.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
                gnre.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
                gnre.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
                gnre.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
                gnre.ConfigGravar("");

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            gnre.ConfigLer(file);

            //Config Geral
            ckbExibirErroSchema.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "ExibirErroSchema");
            txtFormatoAlerta.Text = gnre.ConfigLerValor<string>(ACBrSessao.GNRe, "FormatoAlerta");
            cmbFormaEmissao.SetSelectedValue(gnre.ConfigLerValor<TipoEmissao>(ACBrSessao.GNRe, "FormaEmissao"));
            cmbVersaoDF.SetSelectedValue(gnre.ConfigLerValor<VersaoGNRe>(ACBrSessao.GNRe, "VersaoDF"));
            ckbRetirarAcentos.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "RetirarAcentos");
            ckbSalvar.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "SalvarWS");
            txtLogs.Text = gnre.ConfigLerValor<string>(ACBrSessao.GNRe, "PathSalvar");
            txtSchemaPath.Text = gnre.ConfigLerValor<string>(ACBrSessao.GNRe, "PathSchemas");

            //Config Webservice
            cmbUfDestino.SelectedItem = gnre.ConfigLerValor<string>(ACBrSessao.DFe, "UF");
            cmbSSlType.SetSelectedValue(gnre.ConfigLerValor<SSLType>(ACBrSessao.GNRe, "SSLType"));
            nudTimeOut.Value = gnre.ConfigLerValor<decimal>(ACBrSessao.GNRe, "Timeout");

            var ambiente = gnre.ConfigLerValor<TipoAmbiente>(ACBrSessao.GNRe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "Visualizar");
            ckbSalvarSOAP.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "SalvarWS");
            ckbAjustarAut.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "AjustaAguardaConsultaRet");
            nudAguardar.Value = gnre.ConfigLerValor<int>(ACBrSessao.GNRe, "AguardarConsultaRet");
            nudTentativas.Value = gnre.ConfigLerValor<int>(ACBrSessao.GNRe, "Tentativas");
            nudIntervalos.Value = gnre.ConfigLerValor<int>(ACBrSessao.GNRe, "IntervaloTentativas");
            txtProxyServidor.Text = gnre.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = gnre.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = gnre.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = gnre.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");

            //Config Certificado
            cmbCrypt.SetSelectedValue(gnre.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(gnre.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(gnre.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = gnre.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtDadosPFX.Text = gnre.ConfigLerValor<string>(ACBrSessao.DFe, "DadosPFX");
            txtCertPassword.Text = gnre.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = gnre.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");

            //Config Arquivos
            ckbSalvarArqs.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "SalvarGer");
            ckbPastaMensal.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "SepararPorMes");
            ckbAdicionaLiteral.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "AdicionarLiteral");
            ckbEmissaoPathGNRe.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "EmissaoPathGNRe");
            ckbSepararPorCNPJ.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "SepararPorCNPJ");
            ckbSepararPorModelo.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.GNRe, "SepararPorModelo");
            txtArqGNRe.Text = gnre.ConfigLerValor<string>(ACBrSessao.GNRe, "PathGNRe");

            //Config Email
            txtNome.Text = gnre.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = gnre.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = gnre.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = gnre.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = gnre.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = gnre.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = gnre.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        private void CheckGNReLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibGNRe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                gnre.LimparLista();

            if (xml)
                CarregarGNreXml();
            else
                CarregarGNreIni();
        }

        private void CarregarGNreIni()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Ini GNRe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            gnre.CarregarINI(arquivoIni);
        }

        private void CarregarGNreXml()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Xml GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            gnre.CarregarXML(arquivoIni);
        }

        #endregion Methods

        #region EventHandlers

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void BtnSalvar_Click(object sender, EventArgs e)
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

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void BtnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void BtnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(txtDadosPFX.Text))
            {
                var cert = new X509Certificate2(Convert.FromBase64String(txtDadosPFX.Text), txtCertPassword.Text);
                MessageBox.Show(cert.SerialNumber);
            }

            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            var ret = gnre.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }

        private void btnArqGNRe_Click(object sender, EventArgs e)
        {
            txtArqGNRe.Text = Helpers.SelectFolder();
        }

        private void btnConfiguracaoUF_Click(object sender, EventArgs e)
        {
            try
            {
                var uf = "SP";
                var receita = "";

                if (InputBox.Show("WebServices Consultar: Configuração UF", "UF", ref uf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Consultar: Configuração UF", "Receita", ref receita) != DialogResult.OK) return;
                if (!int.TryParse(receita, out var eReceita)) return;

                rtbRespostas.AppendText(gnre.Consultar(uf, eReceita));
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarXml_Click(object sender, EventArgs e)
        {
            try
            {
                gnre.LimparLista();
                CarregarGNreIni();

                gnre.Assinar();
                var ret = gnre.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviar_Click(object sender, EventArgs e)
        {
            try
            {
                CheckGNReLista();

                var ret = gnre.Enviar();
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarIni_Click(object sender, EventArgs e)
        {
            try
            {
                CheckGNReLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarXml_Click(object sender, EventArgs e)
        {
            try
            {
                CheckGNReLista(true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparLista_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibGNRe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    gnre.LimparLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimir_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                gnre.LimparListaGuiaRetorno();
                gnre.CarregarGuiaRetorno(arquivoXml);
                gnre.Imprimir(MostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                gnre.LimparListaGuiaRetorno();
                gnre.CarregarGuiaRetorno(arquivoXml);
                gnre.ImprimirPDF();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                gnre.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion EventHandlers
    }
}