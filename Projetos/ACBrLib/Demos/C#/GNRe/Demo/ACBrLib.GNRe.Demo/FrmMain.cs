using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.GNRe;

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
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando...");

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
                gnre.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                gnre.Config.Principal.LogPath = logPath;
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
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                gnre.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                gnre.Config.FormatoAlerta = txtFormatoAlerta.Text;
                gnre.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                gnre.Config.VersaoDF = cmbVersaoDF.GetSelectedValue<VersaoGNRe>();
                gnre.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                gnre.Config.SalvarWS = ckbSalvar.Checked;
                gnre.Config.PathSalvar = txtLogs.Text;
                gnre.Config.PathSchemas = txtSchemaPath.Text;

                //Config Webservice
                gnre.Config.DFe.UF = cmbUfDestino.Text;
                gnre.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                gnre.Config.Timeout = (int)nudTimeOut.Value;
                gnre.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                gnre.Config.Visualizar = ckbVisualizar.Checked;
                gnre.Config.SalvarWS = ckbSalvarSOAP.Checked;
                gnre.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
                gnre.Config.AguardarConsultaRet = (int)nudAguardar.Value;
                gnre.Config.Tentativas = (int)nudTentativas.Value;
                gnre.Config.IntervaloTentativas = (int)nudIntervalos.Value;
                gnre.Config.Proxy.Servidor = txtProxyServidor.Text;
                gnre.Config.Proxy.Porta = nudProxyPorta.Text;
                gnre.Config.Proxy.Usuario = txtProxyUsuario.Text;
                gnre.Config.Proxy.Senha = txtProxySenha.Text;

                //Config Certificado
                gnre.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                gnre.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                gnre.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                gnre.Config.DFe.ArquivoPFX = txtCertPath.Text;
                gnre.Config.DFe.Senha = txtCertPassword.Text;
                gnre.Config.DFe.NumeroSerie = txtCertNumero.Text;

                //Config Arquivos
                gnre.Config.SalvarGer = ckbSalvarArqs.Checked;
                gnre.Config.SepararPorMes = ckbPastaMensal.Checked;
                gnre.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                gnre.Config.EmissaoPathGNRe = ckbEmissaoPathGNRe.Checked;
                gnre.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
                gnre.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
                gnre.Config.PathGNRe = txtArqGNRe.Text;

                //Config Email
                gnre.Config.Email.Nome = txtNome.Text;
                gnre.Config.Email.Conta = txtEmail.Text;
                gnre.Config.Email.Usuario = txtUsuario.Text;
                gnre.Config.Email.Senha = txtSenha.Text;
                gnre.Config.Email.Servidor = txtHost.Text;
                gnre.Config.Email.Porta = nudPorta.Text;
                gnre.Config.Email.SSL = ckbSSL.Checked;
                gnre.Config.Email.TLS = ckbTLS.Checked;
                gnre.ConfigGravar();

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
            ckbExibirErroSchema.Checked = gnre.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = gnre.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(gnre.Config.FormaEmissao);
            cmbVersaoDF.SetSelectedValue(gnre.Config.VersaoDF);
            ckbRetirarAcentos.Checked = gnre.Config.RetirarAcentos;
            ckbSalvar.Checked = gnre.Config.SalvarWS;
            txtLogs.Text = gnre.Config.PathSalvar;
            txtSchemaPath.Text = gnre.Config.PathSchemas;

            //Config Webservice
            cmbUfDestino.SelectedItem = gnre.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(gnre.Config.SSLType);
            nudTimeOut.Value = gnre.Config.Timeout;

            var ambiente = gnre.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = gnre.Config.Visualizar;
            ckbSalvarSOAP.Checked = gnre.Config.SalvarWS;
            ckbAjustarAut.Checked = gnre.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = gnre.Config.AguardarConsultaRet;
            nudTentativas.Value = gnre.Config.Tentativas;
            nudIntervalos.Value = gnre.Config.IntervaloTentativas;
            txtProxyServidor.Text = gnre.Config.Proxy.Servidor;
            nudProxyPorta.Text = gnre.Config.Proxy.Porta;
            txtProxyUsuario.Text = gnre.Config.Proxy.Usuario;
            txtProxySenha.Text = gnre.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(gnre.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(gnre.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(gnre.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = gnre.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = gnre.Config.DFe.Senha;
            txtCertNumero.Text = gnre.Config.DFe.NumeroSerie;

            //Config Arquivos
            ckbSalvarArqs.Checked = gnre.Config.SalvarGer;
            ckbPastaMensal.Checked = gnre.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = gnre.Config.AdicionarLiteral;
            ckbEmissaoPathGNRe.Checked = gnre.Config.EmissaoPathGNRe;
            ckbSepararPorCNPJ.Checked = gnre.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = gnre.Config.SepararPorModelo;
            txtArqGNRe.Text = gnre.Config.PathGNRe;

            //Config Email
            txtNome.Text = gnre.Config.Email.Nome;
            txtEmail.Text = gnre.Config.Email.Conta;
            txtUsuario.Text = gnre.Config.Email.Usuario;
            txtSenha.Text = gnre.Config.Email.Senha;
            txtHost.Text = gnre.Config.Email.Servidor;
            nudPorta.Text = gnre.Config.Email.Porta;
            ckbSSL.Checked = gnre.Config.Email.SSL;
            ckbTLS.Checked = gnre.Config.Email.TLS;
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

        private void btnClasseAltoNivel_Click(object sender, EventArgs e)
        {
            var GuiaGNRe = AlimentarDados();

            gnre.LimparLista();
            gnre.CarregarINI(GuiaGNRe);

            try
            {
                var ret = gnre.Enviar();
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

        private string AlimentarDados()
        {
            var gerarGuia = new GerarGuia();

            //Emitente
            gerarGuia.Emitente.Tipo = Tipo.CNPJ;
            gerarGuia.Emitente.IE = "99999999999";
            gerarGuia.Emitente.ID = "99999999999999";
            gerarGuia.Emitente.RazaoSocial = "PROJETO ACBR CONSULTORIA LTDA";
            gerarGuia.Emitente.Endereco = "Rua Cel Aureliano Camargo";
            gerarGuia.Emitente.Cidade = "54003";
            gerarGuia.Emitente.UF = DFeUF.SP;
            gerarGuia.Emitente.CEP = "18270000";
            gerarGuia.Emitente.Telefone = "(11)9999-9999";

            //Complemento
            gerarGuia.Complemento.IdentificadorGuia = "1";
            gerarGuia.Complemento.TipoDocOrigem = 1;
            gerarGuia.Complemento.DocOrigem = 5567;
            gerarGuia.Complemento.DetalhamentoReceita = "";
            gerarGuia.Complemento.Produto = "";

            //Referencia
            gerarGuia.Referencia.Convenio = "";
            gerarGuia.Referencia.Receita = "";
            gerarGuia.Referencia.UfFavorecida = DFeUF.SP;
            gerarGuia.Referencia.DataVencimento = DateTime.Now;
            gerarGuia.Referencia.DataPagamento = DateTime.Now;
            gerarGuia.Referencia.ReferenciaAno = "";
            gerarGuia.Referencia.ReferenciaMes = "";
            gerarGuia.Referencia.ReferenciaParcela = "";
            gerarGuia.Referencia.ReferenciaPeriodo = "";
            gerarGuia.Referencia.ValorTotal = 200;
            gerarGuia.Referencia.ValorPrincipal = 0;

            //Destinatario
            gerarGuia.Destinatario.Tipo = Tipo.CNPJ;
            gerarGuia.Destinatario.IE = "99999999999";
            gerarGuia.Destinatario.ID = "99999999999999";
            gerarGuia.Destinatario.RazaoSocial = "PROJETO ACBR CONSULTORIA LTDA";
            gerarGuia.Destinatario.Cidade = "Tatuí";

            //CampoExtra
            gerarGuia.CampoExtra.Codigo = 1;
            gerarGuia.CampoExtra.Tipo = "";
            gerarGuia.CampoExtra.Valor = 200;

            return gerarGuia.ToString();
        }

        #endregion EventHandlers

    }
}