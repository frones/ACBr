using System;
using System.IO;
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
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "Ambiente", $"{(rdbHomologacao.Checked ? "1" : "0")}");
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
            ACBrCTe.ConfigGravarValor(ACBrSessao.CTe, "Timeout", nudTimeOut.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
            ACBrCTe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);
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
            txtNome.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = ACBrCTe.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = ACBrCTe.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = ACBrCTe.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
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
                rtbRespostas.AppendText(ACBrCTe.StatusServico());
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
                var arquivoIni = Helpers.OpenFile("Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrCTe.LimparLista();
                ACBrCTe.CarregarINI(arquivoIni);

                var ret = ACBrCTe.Enviar(1);
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrCTe.LimparLista();
                ACBrCTe.CarregarXML(arquivoXml);
                ACBrCTe.Imprimir();
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
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
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
                if (InputBox.Show("WebServices Consultar", "Chave da NF-e:", ref chaveOuNFe) != DialogResult.OK) return;
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

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
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

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recib", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrCTe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelarCTe_Click(object sender, EventArgs e)
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

        private void btnInutilizar_Click(object sender, EventArgs e)
        {
            try
            {
                var ano = 1;
                var modelo = 55;
                var serie = 1;
                var numeroInicial = 1;
                var numeroFinal = 1;
                var aJustificativa = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Inutilização", "Ano", ref ano) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Inutilização", "Modelo", ref modelo) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Inutilização", "Serie", ref serie) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Inutilização", "Número Inicial", ref numeroInicial) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Inutilização", "Número Final", ref numeroFinal) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Inutilização", "CNPJ ou o CPF do autor do Emitente", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Inutilização", "Justificativa", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrCTe.Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal);
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