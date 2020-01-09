using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.CTe;
using ACBrLib.Core.DFe;

namespace ACBrLib.CTe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrCTe ACBrCTe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            ACBrCTe = new ACBrCTe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            ACBrCTe.Dispose();
            ACBrCTe = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbVersao.EnumDataSource(VersaoCTe.ve300);
            cmbModeloDocumento.EnumDataSource(ModeloCTe.moCTe);
            cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
            cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
            cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

            cmbUfDestino.SelectedItem = "SP";
            cmbSSlType.EnumDataSource(SSLType.LT_all);

            // Altera as config de log
            ACBrCTe.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ACBrCTe.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
            ACBrCTe.ConfigGravar();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "VersaoDF", cmbVersao.GetSelectedValue<VersaoCTe>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "ModeloDF", cmbModeloDocumento.GetSelectedValue<ModeloCTe>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib", cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "DadosPFX", txtDadosPFX.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "PathSchemas", txtSchemaPath.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "Ambiente", rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao);

            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "Timeout", nudTimeOut.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);

            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SalvarGer", ckbSalvarArqs.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SepararPorMes", ckbPastaMensal.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "AdicionarLiteral", ckbAdicionaLiteral.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "EmissaoPathCTe", ckbEmissaoPathCTe.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SalvarArq", ckbSalvaPathEvento.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SepararPorCNPJ", ckbSepararPorCNPJ.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SepararPorModelo", ckbSepararPorModelo.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "PathCTe", txtArqCTe.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "PathInu", txtArqInu.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "PathEvento", txtArqEvento.Text);

            ACBrCTe.ConfigGravarValor(ACBrSessao.DACTe, "PathLogo", txtLogomarca.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.DACTe, "TipoDACTe", rdbRetrato.Checked ? TipoImpressao.tiRetrato : TipoImpressao.tiPaisagem);

            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
            ACBrCTe.ConfigGravar("");
        }

        private void LoadConfig()
        {
            ACBrCTe.ConfigLer();

            cmbVersao.SetSelectedValue(ACBrCTe.ConfigLerValor<VersaoCTe>(ACBrSessao.CTe, "VersaoDF"));
            cmbModeloDocumento.SetSelectedValue(ACBrCTe.ConfigLerValor<ModeloCTe>(ACBrSessao.CTe, "ModeloDF"));
            cmbCrypt.SetSelectedValue(ACBrCTe.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(ACBrCTe.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(ACBrCTe.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtDadosPFX.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.DFe, "DadosPFX");
            txtCertPassword.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");
            txtSchemaPath.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.CTe, "PathSchemas");
            cmbUfDestino.SelectedItem = ACBrCTe.ConfigLerValor<string>(ACBrSessao.DFe, "UF");

            var ambiente = ACBrCTe.ConfigLerValor<TipoAmbiente>(ACBrSessao.CTe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            cmbSSlType.SetSelectedValue(ACBrCTe.ConfigLerValor<SSLType>(ACBrSessao.CTe, "SSLType"));
            nudTimeOut.Value = ACBrCTe.ConfigLerValor<decimal>(ACBrSessao.CTe, "Timeout");
            txtProxyServidor.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");

            ckbSalvarArqs.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "SalvarGer");
            ckbPastaMensal.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "SepararPorMes");
            ckbAdicionaLiteral.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "AdicionarLiteral");
            ckbEmissaoPathCTe.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "EmissaoPathCTe");
            ckbSalvaPathEvento.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "SalvarArq");
            ckbSepararPorCNPJ.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "SepararPorCNPJ");
            ckbSepararPorModelo.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.CTe, "SepararPorModelo");
            txtArqCTe.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.CTe, "PathCTe");
            txtArqInu.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.CTe, "PathInu");
            txtArqEvento.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.CTe, "PathEvento");

            txtLogomarca.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.DACTe, "PathLogo");
            var tipoImpressao = ACBrCTe.ConfigLerValor<TipoImpressao>(ACBrSessao.DACTe, "TipoDACTe");
            rdbRetrato.Checked = tipoImpressao == TipoImpressao.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoImpressao.tiPaisagem;

            txtNome.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = ACBrCTe.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        private void CheckCTeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibCTe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrCTe.LimparLista();

            if (xml)
                CarregarCTeXml();
            else
                CarregarCTeIni();
        }

        private void CarregarCTeIni()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Ini CTe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrCTe.CarregarINI(arquivoIni);
        }

        private void CarregarCTeXml()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrCTe.CarregarXML(arquivoIni);
        }

        #endregion Methods

        #region EventHandlers

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
            var ret = ACBrCTe.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void btnArqCTe_Click(object sender, EventArgs e)
        {
            txtArqCTe.Text = Helpers.SelectFolder();
        }

        private void btnArqInu_Click(object sender, EventArgs e)
        {
            txtArqInu.Text = Helpers.SelectFolder();
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
            try
            {
                ACBrCTe.LimparLista();
                CarregarCTeIni();

                ACBrCTe.Assinar();
                var ret = ACBrCTe.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarSincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckCTeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrCTe.Enviar(aLote, sincrono: true);
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
                CheckCTeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrCTe.Enviar(aLote);
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
                CheckCTeLista();
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
                CheckCTeLista(true);
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
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibCTe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrCTe.LimparLista();
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.LimparLista();
                ACBrCTe.CarregarXML(arquivoXml);
                ACBrCTe.Imprimir(bMostrarPreview: true);
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.LimparLista();
                ACBrCTe.CarregarXML(arquivoXml);
                ACBrCTe.Imprimir(bMostrarPreview: true);
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrCTe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
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
                CheckCTeLista(true);

                ACBrCTe.Assinar();
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
                CheckCTeLista(true);

                rtbRespostas.AppendText(ACBrCTe.ValidarRegrasdeNegocios());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnStatusServ_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrCTe.StatusServico());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaXml_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrCTe.LimparLista();

                var ret = ACBrCTe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaChave_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da CT-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrCTe.LimparLista();
                var ret = ACBrCTe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recibo", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrCTe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCadastro_Click(object sender, EventArgs e)
        {
            try
            {
                var uf = "SP";
                var documento = "";

                if (InputBox.Show("WebServices Consultar: Cadastro", "UF", ref uf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Consultar: Cadastro", "Documento", ref documento) != DialogResult.OK) return;
                var ie = MessageBox.Show(@"O documento é uma inscrição estadual ?", @"WebServices Consultar: Cadastro", MessageBoxButtons.YesNo) == DialogResult.Yes;

                var ret = ACBrCTe.ConsultaCadastro(uf, documento, ie);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                var aJustificativa = "";
                var eChave = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da CT-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrCTe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                if (InputBox.Show("WebServices Eventos: Enviar", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;

                var ret = ACBrCTe.EnviarEvento(idLote);
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
                var arquivoIni = Helpers.OpenFile("Arquivo Ini CTe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrCTe.CarregarEventoINI(arquivoIni);
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
                if (MessageBox.Show(@"Limpar a lista de eventos ?", @"ACBrLibCTe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrCTe.LimparListaEventos();
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

                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirEvento(arquivoXml, arquivoXmlEvento);
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

                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirEventoPDF(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmailEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrCTe.EnviarEmailEvento(destinatario, arquivoXmlEvento, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnInutilizar_Click(object sender, EventArgs e)
        {
            try
            {
                var ano = 1;
                var modelo = 57;
                var serie = 1;
                var numeroInicial = 1;
                var numeroFinal = 1;
                var aJustificativa = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices: Inutilização", "Ano", ref ano) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Modelo", ref modelo) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Serie", ref serie) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Número Inicial", ref numeroInicial) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Número Final", ref numeroFinal) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "CNPJ ou o CPF do emitente", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Justificativa", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrCTe.Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacao_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirInutilizacao(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacaoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirInutilizacaoPDF(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorChave_Click(object sender, EventArgs e)
        {
            try
            {
                var codUf = 35;
                var cnpj = "";
                var chave = "";

                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Chave da CTe", ref chave) != DialogResult.OK) return;

                var ret = ACBrCTe.DistribuicaoDFePorChave(codUf, cnpj, chave);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorNSU_Click(object sender, EventArgs e)
        {
            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrCTe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret);
        }

        private void btnDFePorUltNSU_Click(object sender, EventArgs e)
        {
            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrCTe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret);
        }

        #endregion EventHandlers
    }
}