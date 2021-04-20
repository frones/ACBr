using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.MDFe;

namespace ACBrLib.MDFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrMDFe ACBrMDFe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            ACBrMDFe = new ACBrMDFe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            ACBrMDFe.Dispose();
            ACBrMDFe = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();

            try
            {
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbVersaoDF.EnumDataSource(VersaoMDFe.ve300);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);

                // Altera as config de log
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrMDFe.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
                ACBrMDFe.ConfigGravar();

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
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "ExibirErroSchema", ckbExibirErroSchema.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "FormatoAlerta", txtFormatoAlerta.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "FormaEmissao", cmbFormaEmissao.GetSelectedValue<TipoEmissao>());
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "VersaoDF", cmbVersaoDF.GetSelectedValue<VersaoMDFe>());
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "RetirarAcentos", ckbRetirarAcentos.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SalvarWS", ckbSalvar.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "PathSalvar", txtLogs.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "PathSchemas", txtSchemaPath.Text);

                //Config Webservice
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "Timeout", nudTimeOut.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "Ambiente", rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "Visualizar", ckbVisualizar.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SalvarWS", ckbSalvarSOAP.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "AjustaAguardaConsultaRet", ckbAjustarAut.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "AguardarConsultaRet", (int)nudAguardar.Value);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "Tentativas", (int)nudTentativas.Value);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "IntervaloTentativas", (int)nudIntervalos.Value);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);

                //Config Certificado
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib", cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "DadosPFX", txtDadosPFX.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);

                //Config Arquivos
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SalvarGer", ckbSalvarArqs.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SepararPorMes", ckbPastaMensal.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "AdicionarLiteral", ckbAdicionaLiteral.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "EmissaoPathMDFe", ckbEmissaoPathNFe.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SalvarArq", ckbSalvaPathEvento.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SepararPorCNPJ", ckbSepararPorCNPJ.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SepararPorModelo", ckbSepararPorModelo.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "PathMDFe", txtArqMDFe.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "PathEvento", txtArqEvento.Text);

                //Config Documento Auxiliar
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DAMDFe, "PathLogo", txtLogomarca.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.DAMDFe, "TipoDAMDFe", rdbRetrato.Checked ? TipoDAMDFE.tiRetrato : TipoDAMDFE.tiPaisagem);

                //Config Email
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
                ACBrMDFe.ConfigGravar("");

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig(string file = "ACBrlib.ini")
        {
            ACBrMDFe.ConfigLer();

            //Config Geral
            ckbExibirErroSchema.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "ExibirErroSchema");
            txtFormatoAlerta.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.MDFe, "FormatoAlerta");
            cmbFormaEmissao.SetSelectedValue(ACBrMDFe.ConfigLerValor<TipoEmissao>(ACBrSessao.MDFe, "FormaEmissao"));
            cmbVersaoDF.SetSelectedValue(ACBrMDFe.ConfigLerValor<VersaoMDFe>(ACBrSessao.MDFe, "VersaoDF"));
            ckbRetirarAcentos.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "RetirarAcentos");
            ckbSalvar.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SalvarWS");
            txtLogs.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.MDFe, "PathSalvar");
            txtSchemaPath.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.MDFe, "PathSchemas");

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "UF");
            cmbSSlType.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLType>(ACBrSessao.MDFe, "SSLType"));
            nudTimeOut.Value = ACBrMDFe.ConfigLerValor<decimal>(ACBrSessao.MDFe, "Timeout");

            var ambiente = ACBrMDFe.ConfigLerValor<TipoAmbiente>(ACBrSessao.MDFe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "Visualizar");
            ckbSalvarSOAP.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SalvarWS");
            ckbAjustarAut.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "AjustaAguardaConsultaRet");
            nudAguardar.Value = ACBrMDFe.ConfigLerValor<int>(ACBrSessao.MDFe, "AguardarConsultaRet");
            nudTentativas.Value = ACBrMDFe.ConfigLerValor<int>(ACBrSessao.MDFe, "Tentativas");
            nudIntervalos.Value = ACBrMDFe.ConfigLerValor<int>(ACBrSessao.MDFe, "IntervaloTentativas");
            txtProxyServidor.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtDadosPFX.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "DadosPFX");
            txtCertPassword.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SalvarGer");
            ckbPastaMensal.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SepararPorMes");
            ckbAdicionaLiteral.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "AdicionarLiteral");
            ckbEmissaoPathNFe.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "EmissaoPathMDFe");
            ckbSalvaPathEvento.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SalvarArq");
            ckbSepararPorCNPJ.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SepararPorCNPJ");
            ckbSepararPorModelo.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.MDFe, "SepararPorModelo");
            txtArqMDFe.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.MDFe, "PathMDFe");
            txtArqEvento.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.MDFe, "PathEvento");

            //Config Documento Auxiliar
            txtLogomarca.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DAMDFe, "PathLogo");
            var tipoImpressao = ACBrMDFe.ConfigLerValor<TipoDAMDFE>(ACBrSessao.DAMDFe, "TipoDAMDFe");
            rdbRetrato.Checked = tipoImpressao == TipoDAMDFE.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoDAMDFE.tiPaisagem;

            //Config Email
            txtNome.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = ACBrMDFe.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        private void CheckMDFeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibMDFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrMDFe.LimparLista();

            if (xml)
                CarregarMDFeXml();
            else
                CarregarMDFeIni();
        }

        public bool validacaoEmail()
        {
            if (txtHost.Text == "")
            {
                errorProvider.SetError(txtHost, "Informe Host SMTP");
                return false;
            }
            else if (txtUsuario.Text == "")
            {
                errorProvider.SetError(txtUsuario, "Informe Usuário");
                return false;
            }
            else if (txtSenha.Text == "")
            {
                errorProvider.SetError(txtSenha, "Informe Senha");
                return false;
            }
            else if (txtNome.Text == "")
            {
                errorProvider.SetError(txtNome, "Informe Nome do Proprietario do e-mail");
                return false;
            }
            else if (txtEmail.Text == "")
            {
                errorProvider.SetError(txtEmail, "Informe e-mail do Proprietario");
                return false;
            }
            else if (nudPorta.Value == 0)
            {
                errorProvider.SetError(nudPorta, "Informe porta de conexão");
                return false;
            }
            else if (ckbSSL.Checked == false && ckbTLS.Checked == false)
            {
                errorProvider.SetError(ckbSSL, "Informe o certificado SSL");
                errorProvider.SetError(ckbTLS, "Informe o certificado TLS");
                return false;
            }
            {
                return true;
            }
        }
        public bool validacao()
        {
            if (txtSchemaPath.Text == "")
            {
                errorProvider.SetError(txtSchemaPath, "Informe Path com Schema");
                return false;
            }
            else if (txtCertPath.Text == "")
            {
                errorProvider.SetError(txtCertPath, "Informe o certificado");
                return false;
            }
            else if (txtCertPassword.Text == "")
            {
                errorProvider.SetError(txtCertPassword, "Informe a senha");
                return false;
            }
            else if (txtCertNumero.Text == "")
            {
                errorProvider.SetError(txtCertNumero, "Informe o número de série");
                return false;
            }
            else if (cmbCrypt.Text == "cryNone")
            {
                errorProvider.SetError(cmbCrypt, "Informe Criptografia");
                return false;
            }
            else if (cmbHttp.Text == "httpNone")
            {
                errorProvider.SetError(cmbHttp, "Informe o tipo SSL");
                return false;
            }
            else if (cmbXmlSign.Text == "xsNone")
            {
                errorProvider.SetError(cmbXmlSign, "Informe assinatura do XML");
                return false;
            }
            else if (cmbSSlType.Text == "LT_all")
            {
                errorProvider.SetError(cmbSSlType, "Informe o tipo SSL");
                return false;
            }
            {
                return true;
            }
        }

        private void CarregarMDFeIni()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrMDFe.CarregarINI(arquivoIni);
        }

        private void CarregarMDFeXml()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrMDFe.CarregarXML(arquivoIni);
        }

        #endregion Methods

        #region EventHandlers

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
            var ret = ACBrMDFe.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }

        private void btnArqMDFe_Click(object sender, EventArgs e)
        {
            txtArqMDFe.Text = Helpers.SelectFolder();
        }

        private void btnArqEvento_Click(object sender, EventArgs e)
        {
            txtArqEvento.Text = Helpers.SelectFolder();
        }

        private void btnLogomarca_Click(object sender, EventArgs e)
        {
            txtLogomarca.Text = Helpers.OpenFile("Image files (*.bmp, *.jpeg, *.png) | *.bmp; *.jpeg; *.png");
        }

        private void btnSalvar_Click(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void btnGerarXml_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                ACBrMDFe.LimparLista();
                CarregarMDFeIni();

                ACBrMDFe.Assinar();
                var ret = ACBrMDFe.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarSincrono_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote, sincrono: true);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarAssincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote);
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
                CheckMDFeLista();
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
                CheckMDFeLista(true);
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
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibMDFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrMDFe.LimparLista();
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.LimparLista();
                ACBrMDFe.CarregarXML(arquivoXml);
                ACBrMDFe.Imprimir(bMostrarPreview: true);
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.LimparLista();
                ACBrMDFe.CarregarXML(arquivoXml);
                ACBrMDFe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            if (!validacaoEmail())
            {
                MessageBox.Show("Erro - Verifique as configurações de E-mail");
                return;
            }

            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrMDFe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnAssinar_Click(object sender, EventArgs e)
        {
            try
            {
                CheckMDFeLista(true);

                ACBrMDFe.Assinar();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnValidarRegra_Click(object sender, EventArgs e)
        {
            try
            {
                CheckMDFeLista(true);

                rtbRespostas.AppendText(ACBrMDFe.ValidarRegrasdeNegocios());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnStatusServ_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                rtbRespostas.AppendText(ACBrMDFe.StatusServico());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaXml_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrMDFe.LimparLista();

                var ret = ACBrMDFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaChave_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da MDF-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrMDFe.LimparLista();
                var ret = ACBrMDFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recibo", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrMDFe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsNaoEncerrados_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var aCNPJ = "";
                if (InputBox.Show("WebServices Consultar: Não Encerrados", "CNPJ.", ref aCNPJ) != DialogResult.OK) return;

                var ret = ACBrMDFe.ConsultaMDFeNaoEnc(aCNPJ);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var idLote = 1;
                var aJustificativa = "";
                var eChave = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da MDF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrMDFe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEncerrar_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var eChave = "";
                var cMunicipio = "";
                if (InputBox.Show("WebServices Eventos: Encerrar", "Chave da MDF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Encerrar", "Código do Municipio", ref cMunicipio) != DialogResult.OK) return;

                var ret = ACBrMDFe.EncerrarMDFe(eChave, DateTime.Now, cMunicipio);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEvento_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var idLote = 1;
                if (InputBox.Show("WebServices Eventos: Enviar", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.EnviarEvento(idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrMDFe.CarregarEventoINI(arquivoIni);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparListaEvento_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista de eventos ?", @"ACBrLibMDFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrMDFe.LimparListaEventos();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.ImprimirEvento(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEventoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.ImprimirEventoPDF(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmailEvento_Click(object sender, EventArgs e)
        {
            if (!validacaoEmail())
            {
                MessageBox.Show("Erro - Verifique as configurações de E-mail");
                return;
            }

            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrMDFe.EnviarEmailEvento(destinatario, arquivoXmlEvento, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorChave_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var codUf = 35;
                var cnpj = "";
                var chave = "";

                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Chave da MDFe", ref chave) != DialogResult.OK) return;

                var ret = ACBrMDFe.DistribuicaoDFePorChave(codUf, cnpj, chave);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorNSU_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrMDFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret);
        }

        private void btnDFePorUltNSU_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrMDFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret);
        }

        #endregion EventHandlers

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void btnGerarChaveMDFe_Click(object sender, EventArgs e)
        {
            try
            {
                var uf = 35;
                if (InputBox.Show("Gerar Chave", "Digite o codigo da UF", ref uf) != DialogResult.OK) return;

                var cod = 45812;
                if (InputBox.Show("Gerar Chave", "Digite o codigo da Númerico", ref cod) != DialogResult.OK) return;

                var doc = 55;
                if (InputBox.Show("Gerar Chave", "Digite o modelo do documento", ref doc) != DialogResult.OK) return;

                var serie = 1;
                if (InputBox.Show("Gerar Chave", "Digite a serie do documento", ref serie) != DialogResult.OK) return;

                var numero = 1;
                if (InputBox.Show("Gerar Chave", "Digite o numero do documento", ref numero) != DialogResult.OK) return;

                var emissao = 1;
                if (InputBox.Show("Gerar Chave", "Digite o tipo de emissão do documento", ref emissao) != DialogResult.OK) return;

                var cnpjCPF = "";
                if (InputBox.Show("Gerar Chave", "Digite o CPF/CNPJ para Gerar a Chave", ref cnpjCPF) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(cnpjCPF)) return;
                 
                rtbRespostas.AppendText(ACBrMDFe.GerarChave(uf, cod, doc, serie, numero, emissao, DateTime.Now, cnpjCPF));
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarAssincrono_Click_1(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote);
                rtbRespostas.AppendText(ret);

            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}