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
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                ACBrMDFe.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBrMDFe.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBrMDFe.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                ACBrMDFe.Config.VersaoDF = cmbVersaoDF.GetSelectedValue<VersaoMDFe>();
                ACBrMDFe.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                ACBrMDFe.Config.SalvarWS = ckbSalvar.Checked;
                ACBrMDFe.Config.PathSalvar = txtLogs.Text;
                ACBrMDFe.Config.PathSchemas = txtSchemaPath.Text;

                //Config Webservice
                ACBrMDFe.Config.DFe.UF = cmbUfDestino.Text;
                ACBrMDFe.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBrMDFe.Config.Timeout = (int)nudTimeOut.Value;
                ACBrMDFe.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBrMDFe.Config.Visualizar = ckbVisualizar.Checked;
                ACBrMDFe.Config.SalvarWS = ckbSalvarSOAP.Checked;
                ACBrMDFe.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
                ACBrMDFe.Config.AguardarConsultaRet = (int)nudAguardar.Value;
                ACBrMDFe.Config.Tentativas = (int)nudTentativas.Value;
                ACBrMDFe.Config.IntervaloTentativas = (int)nudIntervalos.Value;
                ACBrMDFe.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBrMDFe.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrMDFe.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBrMDFe.Config.Proxy.Senha = txtProxySenha.Text;

                //Config Certificado
                ACBrMDFe.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBrMDFe.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBrMDFe.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBrMDFe.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBrMDFe.Config.DFe.Senha = txtCertPassword.Text;
                ACBrMDFe.Config.DFe.NumeroSerie = txtCertNumero.Text;

                //Config Arquivos
                ACBrMDFe.Config.SalvarGer = ckbSalvarArqs.Checked;
                ACBrMDFe.Config.SepararPorMes = ckbPastaMensal.Checked;
                ACBrMDFe.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                ACBrMDFe.Config.EmissaoPathMDFe = ckbEmissaoPathNFe.Checked;
                ACBrMDFe.Config.SalvarArq = ckbSalvaPathEvento.Checked;
                ACBrMDFe.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
                ACBrMDFe.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
                ACBrMDFe.Config.PathMDFe = txtArqMDFe.Text;
                ACBrMDFe.Config.PathEvento = txtArqEvento.Text;

                //Config Documento Auxiliar
                ACBrMDFe.Config.DAMDFe.PathLogo = txtLogomarca.Text;
                ACBrMDFe.Config.DAMDFe.TipoDAMDFe = rdbRetrato.Checked ? TipoDAMDFe.tiRetrato : TipoDAMDFe.tiPaisagem;

                //Config Email
                ACBrMDFe.Config.Email.Nome = txtNome.Text;
                ACBrMDFe.Config.Email.Conta = txtEmail.Text;
                ACBrMDFe.Config.Email.Usuario = txtUsuario.Text;
                ACBrMDFe.Config.Email.Senha = txtSenha.Text;
                ACBrMDFe.Config.Email.Servidor = txtHost.Text;
                ACBrMDFe.Config.Email.Porta = nudPorta.Text;
                ACBrMDFe.Config.Email.SSL = ckbSSL.Checked;
                ACBrMDFe.Config.Email.TLS = ckbTLS.Checked;
                ACBrMDFe.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig(string file = "ACBrlib.ini")
        {
            ACBrMDFe.ConfigLer(file);

            //Config Geral
            ckbExibirErroSchema.Checked = ACBrMDFe.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBrMDFe.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(ACBrMDFe.Config.FormaEmissao);
            cmbVersaoDF.SetSelectedValue(ACBrMDFe.Config.VersaoDF);
            ckbRetirarAcentos.Checked = ACBrMDFe.Config.RetirarAcentos;
            ckbSalvar.Checked = ACBrMDFe.Config.SalvarWS;
            txtLogs.Text = ACBrMDFe.Config.PathSalvar;
            txtSchemaPath.Text = ACBrMDFe.Config.PathSchemas;

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrMDFe.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBrMDFe.Config.SSLType);
            nudTimeOut.Value = ACBrMDFe.Config.Timeout;

            var ambiente = ACBrMDFe.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrMDFe.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBrMDFe.Config.SalvarWS;
            ckbAjustarAut.Checked = ACBrMDFe.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = ACBrMDFe.Config.AguardarConsultaRet;
            nudTentativas.Value = ACBrMDFe.Config.Tentativas;
            nudIntervalos.Value = ACBrMDFe.Config.IntervaloTentativas;
            txtProxyServidor.Text = ACBrMDFe.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrMDFe.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrMDFe.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrMDFe.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrMDFe.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrMDFe.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrMDFe.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrMDFe.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrMDFe.Config.DFe.Senha;
            txtCertNumero.Text = ACBrMDFe.Config.DFe.NumeroSerie;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrMDFe.Config.SalvarGer;
            ckbPastaMensal.Checked = ACBrMDFe.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = ACBrMDFe.Config.AdicionarLiteral;
            ckbEmissaoPathNFe.Checked = ACBrMDFe.Config.EmissaoPathMDFe;
            ckbSalvaPathEvento.Checked = ACBrMDFe.Config.SalvarArq;
            ckbSepararPorCNPJ.Checked = ACBrMDFe.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = ACBrMDFe.Config.SepararPorModelo;
            txtArqMDFe.Text = ACBrMDFe.Config.PathMDFe;
            txtArqEvento.Text = ACBrMDFe.Config.PathEvento;

            //Config Documento Auxiliar
            txtLogomarca.Text = ACBrMDFe.Config.DAMDFe.PathLogo;
            var tipoImpressao = ACBrMDFe.Config.DAMDFe.TipoDAMDFe;
            rdbRetrato.Checked = tipoImpressao == TipoDAMDFe.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoDAMDFe.tiPaisagem;

            //Config Email
            txtNome.Text = ACBrMDFe.Config.Email.Nome;
            txtEmail.Text = ACBrMDFe.Config.Email.Conta;
            txtUsuario.Text = ACBrMDFe.Config.Email.Usuario;
            txtSenha.Text = ACBrMDFe.Config.Email.Senha;
            txtHost.Text = ACBrMDFe.Config.Email.Servidor;
            nudPorta.Text = ACBrMDFe.Config.Email.Porta;
            ckbSSL.Checked = ACBrMDFe.Config.Email.SSL;
            ckbTLS.Checked = ACBrMDFe.Config.Email.TLS;
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

            if (txtUsuario.Text == "")
            {
                errorProvider.SetError(txtUsuario, "Informe Usuário");
                return false;
            }
            if (txtSenha.Text == "")
            {
                errorProvider.SetError(txtSenha, "Informe Senha");
                return false;
            }
            if (txtNome.Text == "")
            {
                errorProvider.SetError(txtNome, "Informe Nome do Proprietario do e-mail");
                return false;
            }
            if (txtEmail.Text == "")
            {
                errorProvider.SetError(txtEmail, "Informe e-mail do Proprietario");
                return false;
            }
            if (nudPorta.Value == 0)
            {
                errorProvider.SetError(nudPorta, "Informe porta de conexão");
                return false;
            }
            if (ckbSSL.Checked == false && ckbTLS.Checked == false)
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

            if (txtCertPath.Text == "")
            {
                errorProvider.SetError(txtCertPath, "Informe o certificado");
                return false;
            }
            if (txtCertPassword.Text == "")
            {
                errorProvider.SetError(txtCertPassword, "Informe a senha");
                return false;
            }
            if (txtCertNumero.Text == "")
            {
                errorProvider.SetError(txtCertNumero, "Informe o número de série");
                return false;
            }
            if (cmbCrypt.Text == "cryNone")
            {
                errorProvider.SetError(cmbCrypt, "Informe Criptografia");
                return false;
            }
            if (cmbHttp.Text == "httpNone")
            {
                errorProvider.SetError(cmbHttp, "Informe o tipo SSL");
                return false;
            }
            if (cmbXmlSign.Text == "xsNone")
            {
                errorProvider.SetError(cmbXmlSign, "Informe assinatura do XML");
                return false;
            }

            if (cmbSSlType.Text != "LT_all") return true;
            errorProvider.SetError(cmbSSlType, "Informe o tipo SSL");
            return false;
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote, sincrono: true);
                rtbRespostas.AppendText(ret.Resposta);
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
                rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro - Verifique as configurações de E-mail");
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                rtbRespostas.AppendText(ACBrMDFe.StatusServico().Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrMDFe.LimparLista();

                var ret = ACBrMDFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da MDF-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrMDFe.LimparLista();
                var ret = ACBrMDFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recibo", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrMDFe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var aCNPJ = "";
                if (InputBox.Show("WebServices Consultar: Não Encerrados", "CNPJ.", ref aCNPJ) != DialogResult.OK) return;

                var ret = ACBrMDFe.ConsultaMDFeNaoEnc(aCNPJ);
                rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
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
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEncerrar_Click(object sender, EventArgs e)
        {
            //if (!validacao())
            //{
            //    MessageBox.Show(@"Erro Verifique as configurações do certificado");
            //    return;
            //}

            //try
            //{
            //    var eChave = "";
            //    var cMunicipio = "";
            //    if (InputBox.Show("WebServices Eventos: Encerrar", "Chave da MDF-e", ref eChave) != DialogResult.OK) return;
            //    if (InputBox.Show("WebServices Eventos: Encerrar", "Código do Municipio", ref cMunicipio) != DialogResult.OK) return;

            //    var ret = ACBrMDFe.EncerrarMDFe(eChave, DateTime.Now, cMunicipio);
            //    rtbRespostas.AppendText(ret.Resposta);
            //}
            //catch (Exception exception)
            //{
            //    MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            //}

            var ret = EncerramentoResposta.LerResposta(File.ReadAllText("Erro.ini"));
            rtbRespostas.AppendText(ret.Resposta);
        }

        private void btnIncCondutor_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var cOrgao = 0;
                var chave = "";
                var condutor = "";
                var cpf = "";
                var lote = 1;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Codigo da UF", ref cOrgao) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Chave da MDF-e", ref chave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Nome do Condutor", ref condutor) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "CPF do Condutor", ref cpf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Número do Lote", ref lote) != DialogResult.OK) return;

                var evento = new EventoIncCondutor
                {
                    cOrgao = cOrgao,
                    chMDFe = chave,
                    xNome = condutor,
                    CPF = cpf,
                    dhEvento = DateTime.Now
                };

                ACBrMDFe.LimparListaEventos();
                ACBrMDFe.CarregarEvento(evento);
                ACBrMDFe.EnviarEvento(lote);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                var idLote = 1;
                if (InputBox.Show("WebServices Eventos: Enviar", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.EnviarEvento(idLote);
                rtbRespostas.AppendText(ret.Resposta);
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

                ACBrMDFe.LimparListaEventos();
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

                ACBrMDFe.LimparListaEventos();
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
                MessageBox.Show(@"Erro - Verifique as configurações de E-mail");
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
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
                rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrMDFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret.Resposta);
        }

        private void btnDFePorUltNSU_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrMDFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret.Resposta);
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
                MessageBox.Show(@"Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}