using System;
using System.IO;
using System.Text;
using System.Windows.Forms;
using ACBrLib;

namespace ACBrLibNFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            var ret = AcbrNFe.NFE_Inicializar("".ToUTF8(), "".ToUTF8());
            AcbrNFe.CheckResult(ret);
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            var ret = AcbrNFe.NFE_Finalizar();
            AcbrNFe.CheckResult(ret);
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbModeloDocumento.SelectedIndex = 0;
            cmbCrypt.SelectedIndex = 3;
            cmbHttp.SelectedIndex = 2;
            cmbXmlSign.SelectedIndex = 4;

            cmbUfDestino.SelectedItem = "SP";
            cmbSSlType.SelectedIndex = 0;

            // Altera as config de log
            var ret = AcbrNFe.NFE_ConfigGravarValor("Principal".ToUTF8(), "LogNivel".ToUTF8(), "4".ToUTF8());
            AcbrNFe.CheckResult(ret);

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ret = AcbrNFe.NFE_ConfigGravarValor("Principal".ToUTF8(), "LogPath".ToUTF8(), logPath.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravar("".ToUTF8());
            AcbrNFe.CheckResult(ret);

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            var ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "ModeloDF".ToUTF8(), cmbModeloDocumento.SelectedIndex.ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "IdCSC".ToUTF8(), txtIdCSC.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "CSC".ToUTF8(), txtCSC.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "SSLCryptLib".ToUTF8(), cmbCrypt.SelectedIndex.ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "SSLHttpLib".ToUTF8(), cmbHttp.SelectedIndex.ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "SSLXmlSignLib".ToUTF8(), cmbXmlSign.SelectedIndex.ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "ArquivoPFX".ToUTF8(), txtCertPath.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "Senha".ToUTF8(), txtCertPassword.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "NumeroSerie".ToUTF8(), txtCertNumero.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "PathSchemas".ToUTF8(), txtSchemaPath.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "UF".ToUTF8(), cmbUfDestino.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "Ambiente".ToUTF8(), $"{(rdbHomologacao.Checked ? "1" : "0")}".ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "SSLType".ToUTF8(), cmbSSlType.SelectedIndex.ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "Timeout".ToUTF8(), nudTimeOut.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "Proxy.Host".ToUTF8(), txtProxyServidor.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "Proxy.Port".ToUTF8(), nudProxyPorta.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "Proxy.User".ToUTF8(), txtProxyUsuario.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "Proxy.Pass".ToUTF8(), txtProxySenha.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "Nome".ToUTF8(), txtNome.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "Conta".ToUTF8(), txtEmail.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "Usuario".ToUTF8(), txtUsuario.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "Senha".ToUTF8(), txtSenha.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "Servidor".ToUTF8(), txtHost.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "Porta".ToUTF8(), nudPorta.Text.ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "SSL".ToUTF8(), Convert.ToInt32(ckbSSL.Checked).ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravarValor("Email".ToUTF8(), "TLS".ToUTF8(), Convert.ToInt32(ckbTLS.Checked).ToString().ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigGravar("".ToUTF8());
            AcbrNFe.CheckResult(ret);
        }

        private void LoadConfig()
        {
            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = AcbrNFe.NFE_ConfigLer("".ToUTF8());
            AcbrNFe.CheckResult(ret);

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "ModeloDF".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            cmbModeloDocumento.SelectedIndex = int.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "IdCSC".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtIdCSC.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "CSC".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtCSC.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "SSLCryptLib".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            cmbCrypt.SelectedIndex = int.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "SSLHttpLib".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            cmbHttp.SelectedIndex = int.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "SSLXmlSignLib".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            cmbXmlSign.SelectedIndex = int.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "ArquivoPFX".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtCertPath.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "Senha".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtCertPassword.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "NumeroSerie".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtCertNumero.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "PathSchemas".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtSchemaPath.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "UF".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            cmbUfDestino.SelectedItem = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "Ambiente".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            var ambiente = buffer.FromUTF8();
            rdbHomologacao.Checked = ambiente == "1";
            rdbProducao.Checked = ambiente == "0";

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "SSLType".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            cmbSSlType.SelectedIndex = int.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("NFe".ToUTF8(), "Timeout".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            nudTimeOut.Value = decimal.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "Proxy.Host".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtProxyServidor.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "Proxy.Port".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            nudProxyPorta.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "Proxy.User".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtProxyUsuario.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("DFe".ToUTF8(), "Proxy.Pass".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtProxySenha.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "Nome".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtNome.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "Conta".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtEmail.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "Usuario".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtUsuario.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "Senha".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtSenha.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "Servidor".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            txtHost.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "Porta".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            nudPorta.Value = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "SSL".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            ckbSSL.Checked = Convert.ToBoolean(Convert.ToInt32(buffer.FromUTF8()));

            bufferLen = 256;
            buffer.Clear();
            buffer.Capacity = bufferLen;

            ret = AcbrNFe.NFE_ConfigLerValor("Email".ToUTF8(), "TLS".ToUTF8(), buffer, ref bufferLen);
            AcbrNFe.CheckResult(ret);

            ckbTLS.Checked = Convert.ToBoolean(Convert.ToInt32(buffer.FromUTF8()));
        }

        #endregion Methods

        #region EventHandlers

        private void BtnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
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
                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                var ret = AcbrNFe.NFE_StatusServico(buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
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

                var ret = AcbrNFe.NFE_LimparLista();
                AcbrNFe.CheckResult(ret);

                ret = AcbrNFe.NFE_CarregarINI(arquivoIni);
                AcbrNFe.CheckResult(ret);

                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                ret = AcbrNFe.NFE_Enviar(1, false, false, false, buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
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

                var ret = AcbrNFe.NFE_LimparLista();
                AcbrNFe.CheckResult(ret);

                ret = AcbrNFe.NFE_CarregarXML(arquivoXml);
                AcbrNFe.CheckResult(ret);

                ret = AcbrNFe.NFE_Imprimir("", 1, "", "1", "", "", "");
                AcbrNFe.CheckResult(ret);
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

                var ret = AcbrNFe.NFE_LimparLista();
                AcbrNFe.CheckResult(ret);

                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                ret = AcbrNFe.NFE_Consultar(chaveOuNFe, buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
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

                var ret = AcbrNFe.NFE_LimparLista();
                AcbrNFe.CheckResult(ret);

                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                ret = AcbrNFe.NFE_Consultar(chaveOuNFe, buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
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

                var ret = AcbrNFe.NFE_EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, "", "", txtMensagem.Text);
                AcbrNFe.CheckResult(ret);
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

                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                var ret = AcbrNFe.NFE_ConsultarRecibo(aRecibo, buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
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

                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                var ret = AcbrNFe.NFE_Cancelar(eChave, aJustificativa, eCNPJ, idLote, buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
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

                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                var ret = AcbrNFe.NFE_Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal, buffer, ref bufferLen);
                AcbrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = AcbrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    AcbrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion EventHandlers
    }
}