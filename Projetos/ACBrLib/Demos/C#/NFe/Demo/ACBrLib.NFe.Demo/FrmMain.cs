using System;
using System.IO;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrNFe AcbrNFe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            AcbrNFe = new ACBrNFe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            AcbrNFe.Dispose();
            AcbrNFe = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbModeloDocumento.EnumDataSource(ModeloDF.moNFe);
            cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
            cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
            cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

            cmbUfDestino.SelectedItem = "SP";
            cmbSSlType.EnumDataSource(SSLType.LT_all);

            // Altera as config de log
            AcbrNFe.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            AcbrNFe.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
            AcbrNFe.ConfigGravar();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "ModeloDF", cmbModeloDocumento.GetSelectedValue<ModeloDF>());
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "IdCSC", txtIdCSC.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "CSC", txtCSC.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib", cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "DadosPFX", txtDadosPFX.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "PathSchemas", txtSchemaPath.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "Ambiente", $"{(rdbHomologacao.Checked ? "1" : "0")}");
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
            AcbrNFe.ConfigGravarValor(ACBrSessao.NFe, "Timeout", nudTimeOut.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
            AcbrNFe.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
            AcbrNFe.ConfigGravar("");
        }

        private void LoadConfig()
        {
            AcbrNFe.ConfigLer();

            cmbModeloDocumento.SetSelectedValue(AcbrNFe.ConfigLerValor<ModeloDF>(ACBrSessao.NFe, "ModeloDF"));
            txtIdCSC.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "IdCSC");
            txtCSC.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "CSC");
            cmbCrypt.SetSelectedValue(AcbrNFe.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(AcbrNFe.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(AcbrNFe.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtDadosPFX.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "DadosPFX");
            txtCertPassword.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");
            txtSchemaPath.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathSchemas");
            cmbUfDestino.SelectedItem = AcbrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "UF");

            var ambiente = AcbrNFe.ConfigLerValor<TipoAmbiente>(ACBrSessao.NFe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            cmbSSlType.SetSelectedValue(AcbrNFe.ConfigLerValor<SSLType>(ACBrSessao.NFe, "SSLType"));
            nudTimeOut.Value = AcbrNFe.ConfigLerValor<decimal>(ACBrSessao.NFe, "Timeout");
            txtProxyServidor.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");
            txtNome.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = AcbrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = AcbrNFe.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = AcbrNFe.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = AcbrNFe.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        #endregion Methods

        #region EventHandlers

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

        private void BtnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void BtnSalvar_Click(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void BtnStatusServ_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(AcbrNFe.StatusServico());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnEnviar_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                AcbrNFe.LimparLista();
                AcbrNFe.CarregarINI(arquivoIni);

                var ret = AcbrNFe.Enviar(1);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnImprimir_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                AcbrNFe.LimparLista();
                AcbrNFe.CarregarXML(arquivoXml);
                AcbrNFe.Imprimir();
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

                AcbrNFe.LimparLista();

                var ret = AcbrNFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnConsultaChave_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da NF-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                AcbrNFe.LimparLista();
                var ret = AcbrNFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                AcbrNFe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnConsultarRecibo_Click(object sender, EventArgs e)
        {
            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recib", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = AcbrNFe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnCancelarNFe_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                var aJustificativa = "";
                var eChave = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da NF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = AcbrNFe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnInutilizar_Click(object sender, EventArgs e)
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

                var ret = AcbrNFe.Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInut_Click(object sender, EventArgs e)
        {
            var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoXml)) return;

            AcbrNFe.ImprimirInutilizacaoPDF(arquivoXml);
        }

        #endregion EventHandlers
    }
}