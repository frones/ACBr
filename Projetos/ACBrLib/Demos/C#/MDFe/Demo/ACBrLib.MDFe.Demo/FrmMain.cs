using System;
using System.IO;
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
            cmbVersao.EnumDataSource(VersaoMDFe.ve300);
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

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "VersaoDF", cmbVersao.GetSelectedValue<VersaoMDFe>());
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib", cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "DadosPFX", txtDadosPFX.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "PathSchemas", txtSchemaPath.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "Ambiente", $"{(rdbHomologacao.Checked ? "1" : "0")}");
            ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
            ACBrMDFe.ConfigGravarValor(ACBrSessao.MDFe, "Timeout", nudTimeOut.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
            ACBrMDFe.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
            ACBrMDFe.ConfigGravar("");
        }

        private void LoadConfig()
        {
            ACBrMDFe.ConfigLer();

            cmbVersao.SetSelectedValue(ACBrMDFe.ConfigLerValor<VersaoMDFe>(ACBrSessao.MDFe, "VersaoDF"));
            cmbCrypt.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtDadosPFX.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "DadosPFX");
            txtCertPassword.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");
            txtSchemaPath.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.MDFe, "PathSchemas");
            cmbUfDestino.SelectedItem = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.DFe, "UF");

            var ambiente = ACBrMDFe.ConfigLerValor<TipoAmbiente>(ACBrSessao.MDFe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            cmbSSlType.SetSelectedValue(ACBrMDFe.ConfigLerValor<SSLType>(ACBrSessao.MDFe, "SSLType"));
            nudTimeOut.Value = ACBrMDFe.ConfigLerValor<decimal>(ACBrSessao.MDFe, "Timeout");
            txtProxyServidor.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");
            txtNome.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = ACBrMDFe.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = ACBrMDFe.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = ACBrMDFe.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
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

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void btnSalvar_Click(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void btnStatusServ_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrMDFe.StatusServico());
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
                var arquivoIni = Helpers.OpenFile("Arquivo Ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrMDFe.LimparLista();
                ACBrMDFe.CarregarINI(arquivoIni);

                var ret = ACBrMDFe.Enviar(1);
                rtbRespostas.AppendText(ret);
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
                ACBrMDFe.Imprimir();
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
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
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
            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave do MDF-e:", ref chaveOuNFe) != DialogResult.OK) return;
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

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
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

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
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

        private void btnCancelar_Click(object sender, EventArgs e)
        {
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

        #endregion EventHandlers
    }
}