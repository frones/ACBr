using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.CTe;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Extensions;

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
            ACBrCTe.Config.Principal.LogNivel = NivelLog.logParanoico;

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ACBrCTe.Config.Principal.LogPath = logPath;
            ACBrCTe.ConfigGravar();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            ACBrCTe.Config.VersaoDF = cmbVersao.GetSelectedValue<VersaoCTe>();
            ACBrCTe.Config.ModeloDF = cmbModeloDocumento.GetSelectedValue<ModeloCTe>();
            ACBrCTe.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
            ACBrCTe.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
            ACBrCTe.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
            ACBrCTe.Config.DFe.ArquivoPFX = txtCertPath.Text;
            ACBrCTe.Config.DFe.Senha = txtCertPassword.Text;
            ACBrCTe.Config.DFe.NumeroSerie = txtCertNumero.Text;
            ACBrCTe.Config.PathSchemas = txtSchemaPath.Text;
            ACBrCTe.Config.DFe.UF = cmbUfDestino.Text;
            ACBrCTe.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;

            ACBrCTe.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
            ACBrCTe.Config.Timeout = (int)nudTimeOut.Value;
            ACBrCTe.Config.Proxy.Servidor = txtProxyServidor.Text;
            ACBrCTe.Config.Proxy.Porta = nudProxyPorta.Text;
            ACBrCTe.Config.Proxy.Usuario = txtProxyUsuario.Text;
            ACBrCTe.Config.Proxy.Senha = txtProxySenha.Text;

            ACBrCTe.Config.SalvarGer = ckbSalvarArqs.Checked;
            ACBrCTe.Config.SepararPorMes = ckbPastaMensal.Checked;
            ACBrCTe.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
            ACBrCTe.Config.EmissaoPathCTe = ckbEmissaoPathCTe.Checked;
            ACBrCTe.Config.SalvarArq = ckbSalvaPathEvento.Checked;
            ACBrCTe.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
            ACBrCTe.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
            ACBrCTe.Config.PathCTe = txtArqCTe.Text;
            ACBrCTe.Config.PathInu = txtArqInu.Text;
            ACBrCTe.Config.PathEvento = txtArqEvento.Text;

            ACBrCTe.Config.DACTe.PathLogo = txtLogomarca.Text;
            ACBrCTe.Config.DACTe.TipoDACTe = rdbRetrato.Checked ? TipoDACTE.tiRetrato : TipoDACTE.tiPaisagem;

            ACBrCTe.Config.Email.Nome = txtNome.Text;
            ACBrCTe.Config.Email.Conta = txtEmail.Text;
            ACBrCTe.Config.Email.Usuario = txtUsuario.Text;
            ACBrCTe.Config.Email.Senha = txtSenha.Text;
            ACBrCTe.Config.Email.Servidor = txtHost.Text;
            ACBrCTe.Config.Email.Porta = nudPorta.Text;
            ACBrCTe.Config.Email.SSL = ckbSSL.Checked;
            ACBrCTe.Config.Email.TLS = ckbTLS.Checked;
            ACBrCTe.ConfigGravar();
        }

        private void LoadConfig(string file = "")
        {
            ACBrCTe.ConfigLer(file);

            cmbVersao.SetSelectedValue(ACBrCTe.Config.VersaoDF);
            cmbModeloDocumento.SetSelectedValue(ACBrCTe.Config.ModeloDF);
            cmbCrypt.SetSelectedValue(ACBrCTe.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrCTe.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrCTe.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrCTe.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrCTe.Config.DFe.Senha;
            txtCertNumero.Text = ACBrCTe.Config.DFe.NumeroSerie;
            txtSchemaPath.Text = ACBrCTe.Config.PathSchemas;
            cmbUfDestino.SelectedItem = ACBrCTe.Config.DFe.UF;

            var ambiente = ACBrCTe.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            cmbSSlType.SetSelectedValue(ACBrCTe.Config.SSLType);
            nudTimeOut.Value = ACBrCTe.Config.Timeout;
            txtProxyServidor.Text = ACBrCTe.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrCTe.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrCTe.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrCTe.Config.Proxy.Senha;

            ckbSalvarArqs.Checked = ACBrCTe.Config.SalvarGer;
            ckbPastaMensal.Checked = ACBrCTe.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = ACBrCTe.Config.AdicionarLiteral;
            ckbEmissaoPathCTe.Checked = ACBrCTe.Config.EmissaoPathCTe;
            ckbSalvaPathEvento.Checked = ACBrCTe.Config.SalvarArq;
            ckbSepararPorCNPJ.Checked = ACBrCTe.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = ACBrCTe.Config.SepararPorModelo;
            txtArqCTe.Text = ACBrCTe.Config.PathCTe;
            txtArqInu.Text = ACBrCTe.Config.PathInu;
            txtArqEvento.Text = ACBrCTe.Config.PathEvento;

            txtLogomarca.Text = ACBrCTe.Config.DACTe.PathLogo;
            var tipoImpressao = ACBrCTe.Config.DACTe.TipoDACTe;
            rdbRetrato.Checked = tipoImpressao == TipoDACTE.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoDACTE.tiPaisagem;

            txtNome.Text = ACBrCTe.Config.Email.Nome;
            txtEmail.Text = ACBrCTe.Config.Email.Conta;
            txtUsuario.Text = ACBrCTe.Config.Email.Usuario;
            txtSenha.Text = ACBrCTe.Config.Email.Senha;
            txtHost.Text = ACBrCTe.Config.Email.Servidor;
            nudPorta.Text = ACBrCTe.Config.Email.Porta;
            ckbSSL.Checked = ACBrCTe.Config.Email.SSL;
            ckbTLS.Checked = ACBrCTe.Config.Email.TLS;
        }

        private void CheckCTeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", "ACBrLibCTe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrCTe.LimparLista();

            if (xml)
                CarregarCTeXml();
            else
                CarregarCTeIni();
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
            else if ((txtCertNumero.Text == "") && (txtCertPath.Text == ""))
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

        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = ACBrCTe.OpenSSLInfo();
            rtbRespostas.AppendText(ret);
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
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                CheckCTeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrCTe.Enviar(aLote, sincrono: true);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarAssincrono_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                CheckCTeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrCTe.Enviar(aLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparLista_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", "ACBrLibCTe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrCTe.LimparLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe(*.xml) | *.xml | Todos os Arquivos(*.*) | *.* ");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.LimparLista();
                ACBrCTe.CarregarXML(arquivoXml);
                ACBrCTe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            if (!validacaoEmail())
            {
                MessageBox.Show("Erro - Verifique as configurações de E - mail");
                return;
            }

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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnAssinar_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

            try
            {
                CheckCTeLista(true);

                ACBrCTe.Assinar();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                rtbRespostas.AppendText(ACBrCTe.StatusServico());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrCTe.LimparLista();

                var ret = ACBrCTe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                if (InputBox.Show("WebServices Consultar", "Chave da CT - e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrCTe.LimparLista();
                var ret = ACBrCTe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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

                var ret = ACBrCTe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCadastro_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da CT-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrCTe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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

                var ret = ACBrCTe.EnviarEvento(idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini CTe(*.ini) | *.ini | Todos os Arquivos(*.*) | *.* ");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrCTe.CarregarEventoINI(arquivoIni);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparListaEvento_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista de eventos ?", "ACBrLibCTe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrCTe.LimparListaEventos();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento(*.xml) | *.xml | Todos os Arquivos(*.*) | *.* ");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirEvento(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEventoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento(*.xml) | *.xml | Todos os Arquivos(*.*) | *.* ");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirEventoPDF(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmailEvento_Click(object sender, EventArgs e)
        {
            if (!validacaoEmail())
            {
                MessageBox.Show("Erro - Verifique as configurações de E - mail");
                return;
            }

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
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnInutilizar_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show("Erro Verifique as configurações do certificado");
                return;
            }

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
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacao_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização(*.xml) | *.xml | Todos os Arquivos(*.*) | *.* ");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirInutilizacao(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacaoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização(*.xml) | *.xml | Todos os Arquivos(*.*) | *.* ");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.ImprimirInutilizacaoPDF(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                if (InputBox.Show("WebServices: Distribuição DFe", "Chave da CTe", ref chave) != DialogResult.OK) return;

                var ret = ACBrCTe.DistribuicaoDFePorChave(codUf, cnpj, chave);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
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

            var ret = ACBrCTe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
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
            var ArquivoOuXml = Helpers.OpenFile("Arquivos Distribuição DFe (*-dist-dfe.xml)|*-dist-dfe.xml|Todos os Arquivos (*.*)|*.*",
                                                "Selecione um Arquivo de Distribuição para simular uma consulta ou feche para consultar o WebService");

            if (string.IsNullOrEmpty(ArquivoOuXml))
            {
                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;
            }

            var ret = ACBrCTe.DistribuicaoDFe(codUf, cnpj, eNsu, ArquivoOuXml);
            rtbRespostas.AppendText(ret);
        }

        #endregion EventHandlers

        private void BtnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void BtnGerarChaveCTe_Click(object sender, EventArgs e)
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
                if (InputBox.Show("Gerar Chave", "Digite o CPF / CNPJ para Gerar a Chave", ref cnpjCPF) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(cnpjCPF)) return;

                rtbRespostas.AppendText(ACBrCTe.GerarChave(uf, cod, doc, serie, numero, emissao, DateTime.Now, cnpjCPF));
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnValidarXmlClick(object sender, EventArgs e)
        {
            try
            {
                CheckCTeLista(true);
                ACBrCTe.Validar();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, "Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

        private void btnClasseAltoNivel_Click(object sender, EventArgs e)
        {

            var cte = AlimentarDados();

            ACBrCTe.LimparLista();
            rtbRespostas.AppendText(cte);
            ACBrCTe.CarregarINI(cte);

            try
            {
                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrCTe.Enviar(aLote);
                rtbRespostas.AppendText(ret.Resposta);

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private string AlimentarDados()
        {
            var cte = new CTe();
            
            //InfCTe
            cte.InfCTe.Versao = "3.0";

            //Identificação
            cte.Identificacao.cUF = "15";
            cte.Identificacao.CFOP = 5353;
            cte.Identificacao.NatOp = "PRESTACAO SERVICO";
            cte.Identificacao.mod = 57;
            cte.Identificacao.serie = 1;
            cte.Identificacao.nCT = 000001402;
            cte.Identificacao.dhEmi = DateTime.Now;
            cte.Identificacao.tpImp = rdbRetrato.Checked ? TipoDACTE.tiRetrato : TipoDACTE.tiPaisagem;
            cte.Identificacao.tpEmis = TipoEmissao.teNormal;
            cte.Identificacao.tpAmb = rdbProducao.Checked ? TipoAmbiente.taProducao : TipoAmbiente.taHomologacao;
            cte.Identificacao.tpCte = TipoCTe.tpCTeNormal;
            cte.Identificacao.ProcEmi = ProcessoEmissao.peAplicativoContribuinte;
            cte.Identificacao.verProc = "3.0";
            cte.Identificacao.cMunEnv = 1501402;
            cte.Identificacao.xMunEnv = "BELEM";
            cte.Identificacao.UFEnv = "PA";
            cte.Identificacao.modal = ModalCTe.moRodoviario;
            cte.Identificacao.tpServ = TipoServicoCTe.tsNormal;
            cte.Identificacao.indIEToma = IndicadorTomadorCTe.contribuinteICMS;
            cte.Identificacao.cMunIni = 3119401;
            cte.Identificacao.xMunIni = "CORONEL FABRICIANO";
            cte.Identificacao.UFIni = "MG";
            cte.Identificacao.cMunFim = 2900207;
            cte.Identificacao.xMunFim = "ABARE";
            cte.Identificacao.UFFim = "BA";
            cte.Identificacao.retira = 0;
            cte.Identificacao.xDetRetira = "";
            cte.Identificacao.indGlobalizado = 0;
            //cte.Identificacao.toma = 3;
            cte.Identificacao.dhCont = DateTime.Now;

            //Tomador
            cte.Tomador4.toma = 4;
            cte.Tomador4.CNPJCPF = "10242141000174";
            cte.Tomador4.IE = "0010834420031";
            cte.Tomador4.xNome = "ACOUGUE E SUPERMERCADO SOUZA LTDA";
            cte.Tomador4.xLgr = "RUA BELO HORIZONTE";
            cte.Tomador4.nro = "614";
            cte.Tomador4.xCpl = "N D";
            cte.Tomador4.xBairro = "CALADINA";
            cte.Tomador4.cMun = 3119401;
            cte.Tomador4.xMun = "CORONEL FABRICIANO";
            cte.Tomador4.CEP = "35171167";
            cte.Tomador4.UF = "MG";
            cte.Tomador4.cPais = 1058;
            cte.Tomador4.xPais = "BRASIL";
            cte.Tomador4.fone = "33445566";

            //Complemento
            cte.Complemento.xCaracAd = "Carac Adic";
            cte.Complemento.xCaracSer = "Carac Adicionais do Serviço";
            cte.Complemento.xEmi = "Nome do Emitente";
            cte.Complemento.TipoData = TipoPeriodoCTe.noPeriodo;
            cte.Complemento.TipoHora = TipoHorarioCTe.noIntervaloTempo;
            cte.Complemento.origCalc = "Sao Paulo";
            cte.Complemento.destCalc = "Campinas";
            cte.Complemento.xObs = "Observação livre";           
           
            var dProg = "23/06/2023";
            cte.Complemento.dProg = DateTime.Parse(dProg);

            var dIni = "20/06/2023";
            cte.Complemento.dIni = DateTime.Parse(dIni);

            var dFim = "25/06/2023";
            cte.Complemento.dFim = DateTime.Parse(dFim);

            DateTime date = DateTime.Now;
            var horaFomatada = date.ToString("HH:mm:ss");
            cte.Complemento.hProg = horaFomatada;
            cte.Complemento.hIni = horaFomatada;
            cte.Complemento.hFim = horaFomatada;

            var obsCont = new ObsContCTe();
            obsCont.xCampo = "Nome do Campo";
            obsCont.xTexto = "Valor do Campo";
            cte.ObsCont.Add(obsCont);

            var obsFisco = new ObsFiscoCTe();
            obsFisco.xCampo = "Nome do Campo";
            obsFisco.xTexto = "Valor do Campo";
            cte.ObsFisco.Add(obsFisco);

            //Emitente
            cte.Emitente.CNPJ = "18760540000139";
            cte.Emitente.IE = "111111";
            cte.Emitente.xNome = "RAZAO SOCIAL DE TESTE";
            cte.Emitente.xFant = "FANTASIA DE TESTE";
            cte.Emitente.xLgr = "Logradouro";
            cte.Emitente.nro = "1";
            cte.Emitente.xCpl = "Complemento";
            cte.Emitente.xBairro = "Bairro";
            cte.Emitente.cMun = 1501402;
            cte.Emitente.xMun = "BELEM";
            cte.Emitente.CEP = "17250000";
            cte.Emitente.UF = "PA";
            cte.Emitente.fone = "33445566";

            //Remetente
            cte.Remetente.CNPJCPF = "05481336000137";
            cte.Remetente.IE = "12345678";
            cte.Remetente.xNome = "Nome do Remetente";
            cte.Remetente.xFant = "Nome Fantasia";
            cte.Remetente.fone = "33445566";
            cte.Remetente.xLgr = "Rua 1";
            cte.Remetente.nro = "200";
            cte.Remetente.xBairro = "Centro";
            cte.Remetente.cMun = 3554003;
            cte.Remetente.xMun = "Nome do Municipio";
            cte.Remetente.CEP = "14123456";
            cte.Remetente.UF = "SP";
            cte.Remetente.cPais = 1058;
            cte.Remetente.xPais = "BRASIL";
            
            //Destinatário
            cte.Destinatario.CNPJCPF = "05481336000137";
            cte.Destinatario.IE = "12345678";
            cte.Destinatario.xNome = "Nome do Destinatário";
            cte.Destinatario.fone = "33445566";
            cte.Destinatario.xLgr = "Rua 1";
            cte.Destinatario.nro = "200";
            cte.Destinatario.xBairro = "Centro";
            cte.Destinatario.cMun = 3554003;
            cte.Destinatario.xMun = "Nome do Municipio";
            cte.Destinatario.CEP = "14123456";
            cte.Destinatario.UF = "SP";
            cte.Destinatario.cPais = 1058;
            cte.Destinatario.xPais = "BRASIL";
            cte.Destinatario.ISUF = "14123456";

            //Expedidor
            cte.Expedidor.CNPJCPF = "05481336000137";
            cte.Expedidor.IE = "12345678";
            cte.Expedidor.xNome = "Nome do Expedidor";
            cte.Expedidor.fone = "33445566";
            cte.Expedidor.xLgr = "Rua 1";
            cte.Expedidor.nro = "200";
            cte.Expedidor.xBairro = "Centro";
            cte.Expedidor.cMun = 3554003;
            cte.Expedidor.xMun = "Nome do Municipio";
            cte.Expedidor.CEP = "14123456";
            cte.Expedidor.UF = "SP";
            cte.Expedidor.cPais = 1058;
            cte.Expedidor.xPais = "BRASIL";

            //Recebedor
            cte.Recebedor.CNPJCPF = "05481336000137";
            cte.Recebedor.IE = "12345678";
            cte.Recebedor.xNome = "Nome do Recebedor";
            cte.Recebedor.fone = "33445566";
            cte.Recebedor.xLgr = "Rua 1";
            cte.Recebedor.nro = "200";
            cte.Recebedor.xBairro = "Centro";
            cte.Recebedor.cMun = 3554003;
            cte.Recebedor.xMun = "Nome do Municipio";
            cte.Recebedor.CEP = "14123456";
            cte.Recebedor.UF = "SP";
            cte.Recebedor.cPais = 1058;
            cte.Recebedor.xPais = "BRASIL";

            cte.ValoresPrestacaoServico.vTPrest = 100;
            cte.ValoresPrestacaoServico.vRec = 100;

            cte.InformacoesRelativasImpostos.vTotTrib = 17;
            cte.InformacoesRelativasImpostos.infAdFisco = "Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preço deste servico é de R$ 17,00 (17,00%) Fonte: IBPT";

            var comp = new ComponentesValorPrestacaoCTe()
            {
                xNome = "DFRNER KRTJ",
                vComp = 100
            };
            cte.ComponentesValorPrestacao.Add(comp);

            cte.InformacoesRelativasImpostos.ICMSSN.CST = CSTCTe.ICMSSN;
            cte.InformacoesRelativasImpostos.ICMSSN.indSN = 1;

            cte.InformacoesRelativasImpostos.vTotTrib = 10;
            cte.InformacoesRelativasImpostos.infAdFisco = "teste";

            cte.GrupoInformacoesNormalSubstituto.infCarga.vCarga = 5000;
            cte.GrupoInformacoesNormalSubstituto.infCarga.proPred = "Produto Predominante";
            cte.GrupoInformacoesNormalSubstituto.infCarga.xOutCat = "Pacotes";
            cte.GrupoInformacoesNormalSubstituto.infCarga.vCargaAverb = 5000;

            var infQ = new InfQCTe();
            infQ.cUnid = CodUniMedidaCTe.UNIDADE;
            infQ.tpMed = "KG";
            infQ.qCarga = 10;

            cte.GrupoInformacoesNormalSubstituto.infCarga.infQ.Add(infQ);

            var NFe = new InfNFeCTe();
            NFe.chave = "42210117089484000190550110000091001371413248";
            NFe.dPrev = DateTime.Parse("30/06/2023");
            
            cte.GrupoInformacoesNormalSubstituto.infDoc.infNFe.Add(NFe);
           
            cte.Rodoviario.RNTRC = "12345678";

            cte.GrupoInformacoesNormalSubstituto.cobr.fat.nFat = "123";
            cte.GrupoInformacoesNormalSubstituto.cobr.fat.vOrig = 100;
            cte.GrupoInformacoesNormalSubstituto.cobr.fat.vDesc = 0;
            cte.GrupoInformacoesNormalSubstituto.cobr.fat.vLiq = 100;


            var dup = new DupCTe()
            {
                nDup = "123",
                dVenc = DateTime.Now,
                vDup = 100
            };
            cte.GrupoInformacoesNormalSubstituto.cobr.dup.Add(dup);

            return cte.ToString();
        }
    }
}