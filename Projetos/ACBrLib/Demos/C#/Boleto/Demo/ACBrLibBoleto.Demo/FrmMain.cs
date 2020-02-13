using System;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Boleto;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLibBoleto.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrBoleto boleto;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            boleto = new ACBrBoleto();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Finalizando a dll
            boleto.Dispose();
            boleto = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbTipoInscricao.EnumDataSource(ACBrPessoa.pJuridica);
            cmbTipoDocumento.EnumDataSource(ACBrTipoDocumento.Tradicional);
            cmbTipoCarteira.EnumDataSource(ACBrTipoCarteira.tctSimples);
            cmbModeloImpressao.EnumDataSource(ACBrBolLayOut.lPadrao);
            cmbBanco.EnumDataSource(ACBrTipoCobranca.cobNenhum);
            cmbRespEmissao.EnumDataSource(ACBrResponEmissao.tbCliEmite);
            cmbLayoutCNAB.EnumDataSource(ACBrLayoutRemessa.c240);

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cmbImpressora.Items.Add(printer);
            }

            if (PrinterSettings.InstalledPrinters.Count > 0)
                cmbImpressora.SelectedIndex = 0;

            // Altera as config de log
            boleto.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", NivelLog.logParanoico);

            var logPath = Path.Combine(Application.StartupPath, "Docs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            boleto.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
            boleto.ConfigGravar();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void LoadConfig()
        {
            boleto.ConfigLer();

            cmbModeloImpressao.SetSelectedValue(boleto.ConfigLerValor<ACBrBolLayOut>(ACBrSessao.BoletoBancoFCFortesConfig, "Layout"));
            chkPreview.Checked = boleto.ConfigLerValor<bool>(ACBrSessao.BoletoBancoFCFortesConfig, "MostrarPreview");
            chkProgresso.Checked = boleto.ConfigLerValor<bool>(ACBrSessao.BoletoBancoFCFortesConfig, "MostrarProgresso");
            chkSetup.Checked = boleto.ConfigLerValor<bool>(ACBrSessao.BoletoBancoFCFortesConfig, "MostrarSetup");
            txtNomeArquivo.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoBancoFCFortesConfig, "NomeArquivo");
            nudCopias.Value = boleto.ConfigLerValor<int>(ACBrSessao.BoletoBancoFCFortesConfig, "NumeroCopias");
            cmbImpressora.SelectedItem = boleto.ConfigLerValor<string>(ACBrSessao.BoletoBancoFCFortesConfig, "PrinterName");
            txtDirLogo.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoBancoFCFortesConfig, "DirLogo");
            cmbBanco.SetSelectedValue(boleto.ConfigLerValor<ACBrTipoCobranca>(ACBrSessao.BoletoBancoConfig, "TipoCobranca"));
            txtAgencia.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Agencia");
            txtDigAgencia.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "AgenciaDigito");
            txtDigAgencia.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "AgenciaDigito");
            txtConta.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Conta");
            txtDigConta.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "ContaDigito");
            txtCodTransmissao.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "CodigoTransmissao");
            txtConvenio.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Convenio");
            txtModalidade.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Modalidade");
            txtCodCedente.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "CodigoCedente");
            cmbRespEmissao.SetSelectedValue(boleto.ConfigLerValor<ACBrResponEmissao>(ACBrSessao.BoletoCedenteConfig, "ResponEmissao"));
            txtBairro.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Bairro");
            txtCEP.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "CEP");
            txtCidade.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Cidade");
            txtCNPJCPF.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "CNPJCPF");
            txtComplemento.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Complemento");
            txtLogradouro.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Logradouro");
            txtNomeRes.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Nome");
            txtNumeroRes.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "NumeroRes");
            txtTelefone.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "Telefone");
            cmbUF.SelectedItem = boleto.ConfigLerValor<string>(ACBrSessao.BoletoCedenteConfig, "UF");
            cmbTipoCarteira.SetSelectedValue(boleto.ConfigLerValor<ACBrTipoCarteira>(ACBrSessao.BoletoCedenteConfig, "TipoCarteira"));
            cmbTipoDocumento.SetSelectedValue(boleto.ConfigLerValor<ACBrTipoDocumento>(ACBrSessao.BoletoCedenteConfig, "TipoDocumento"));
            cmbTipoInscricao.SetSelectedValue(boleto.ConfigLerValor<ACBrPessoa>(ACBrSessao.BoletoCedenteConfig, "TipoInscricao"));
            txtDirRemessa.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoDiretorioConfig, "DirArqRemessa");
            txtDirRetorno.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoDiretorioConfig, "DirArqRetorno");
            cmbLayoutCNAB.SetSelectedValue(boleto.ConfigLerValor<ACBrLayoutRemessa>(ACBrSessao.BoletoDiretorioConfig, "LayoutRemessa"));
            txtNomeRemessa.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoDiretorioConfig, "NomeArqRemessa");
            txtNomeRetorno.Text = boleto.ConfigLerValor<string>(ACBrSessao.BoletoDiretorioConfig, "NomeArqRetorno");
            ckbCedenteRetorno.Checked = boleto.ConfigLerValor<bool>(ACBrSessao.BoletoDiretorioConfig, "LeCedenteRetorno");
            txtNome.Text = boleto.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = boleto.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = boleto.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = boleto.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = boleto.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = boleto.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = boleto.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = boleto.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        private void SaveConfig()
        {
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "Layout", cmbModeloImpressao.GetSelectedValue<ACBrBolLayOut>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "MostrarPreview", chkPreview.Checked);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "MostrarProgresso", chkProgresso.Checked);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "MostrarSetup", chkSetup.Checked);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "NomeArquivo", txtNomeArquivo.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "NumeroCopias", nudCopias.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "PrinterName", cmbImpressora.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoFCFortesConfig, "DirLogo", txtDirLogo.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoBancoConfig, "TipoCobranca", cmbBanco.GetSelectedValue<ACBrTipoCobranca>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Agencia", txtAgencia.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "AgenciaDigito", txtDigAgencia.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Conta", txtConta.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "ContaDigito", txtDigConta.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "CodigoTransmissao", txtCodTransmissao.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Convenio", txtConvenio.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Modalidade", txtModalidade.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "CodigoCedente", txtCodCedente.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "ResponEmissao", cmbRespEmissao.GetSelectedValue<ACBrResponEmissao>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Bairro", txtBairro.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "CEP", txtCEP.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Cidade", txtCidade.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "CNPJCPF", txtCNPJCPF.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Complemento", txtComplemento.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Logradouro", txtLogradouro.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Nome", txtNomeRes.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "NumeroRes", txtNumeroRes.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "Telefone", txtTelefone.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "UF", cmbUF.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "TipoCarteira", cmbTipoCarteira.GetSelectedValue<ACBrTipoCarteira>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "TipoDocumento", cmbTipoDocumento.GetSelectedValue<ACBrTipoDocumento>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoCedenteConfig, "TipoInscricao", cmbTipoInscricao.GetSelectedValue<ACBrPessoa>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoDiretorioConfig, "DirArqRemessa", txtDirRemessa.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoDiretorioConfig, "DirArqRetorno", txtDirRetorno.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoDiretorioConfig, "LayoutRemessa", cmbLayoutCNAB.GetSelectedValue<ACBrLayoutRemessa>());
            boleto.ConfigGravarValor(ACBrSessao.BoletoDiretorioConfig, "NomeArqRemessa", txtNomeRemessa.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoDiretorioConfig, "NomeArqRetorno", txtNomeRetorno.Text);
            boleto.ConfigGravarValor(ACBrSessao.BoletoDiretorioConfig, "LeCedenteRetorno", ckbCedenteRetorno.Checked);
            boleto.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
            boleto.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
            boleto.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
            boleto.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
            boleto.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
            boleto.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
            boleto.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
            boleto.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
            boleto.ConfigGravar();
        }

        #endregion Methods

        #region EventHandlers

        private void BtnDirLogo_Click(object sender, EventArgs e)
        {
            txtDirLogo.Text = Helpers.SelectFolder();
        }

        private void BtnDirRemessa_Click(object sender, EventArgs e)
        {
            txtDirRemessa.Text = Helpers.SelectFolder();
        }

        private void BtnDirRetorno_Click(object sender, EventArgs e)
        {
            txtDirRetorno.Text = Helpers.SelectFolder();
        }

        private void BtnGravarConfig_Click(object sender, EventArgs e)
        {
            SaveConfig();
        }

        private void BtnIncluirTitulo_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Titulo (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = boleto.IncluirTitulos(iniPath);
            rtbRespostas.AppendLine(ret);
        }

        private void BtnImprimir_Click(object sender, EventArgs e)
        {
            boleto.Imprimir();
            rtbRespostas.AppendLine("Boletos impressos.");
        }

        private void BtnGerarRemessa_Click(object sender, EventArgs e)
        {
            string ret = "";
            if (string.IsNullOrEmpty(txtDirRemessa.Text))       
                ret = Application.StartupPath;
            else 
                ret = txtDirRemessa.Text;
            boleto.GerarRemessa(ret, 1, txtNomeRemessa.Text);
            rtbRespostas.AppendLine("Remessa Gerada.");
        }

        private void BtnTotalTitulo_Click(object sender, EventArgs e)
        {
            var ret = boleto.TotalTitulosLista();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnLimparLista_Click(object sender, EventArgs e)
        {
            boleto.LimparLista();
            rtbRespostas.AppendLine("Lista Limpa.");
        }

        #endregion EventHandlers

        private void BtnConfigDados_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Configurar Dados do Cedente (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = boleto.ConfigurarDados(iniPath);
            rtbRespostas.AppendLine(ret);


        }

        private void Button1_Click(object sender, EventArgs e)
        {
            var ret = boleto.ListaOcorrenciasEX();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnGerarPDF_Click(object sender, EventArgs e)
        {
            boleto.GerarPDF();
            rtbRespostas.AppendLine("PDF Gerado");
        }

        private void BtnLinhaDigitavel_Click(object sender, EventArgs e)
        {
            var ret = boleto.RetornaLinhaDigitavel(0);
            rtbRespostas.AppendLine(ret);
        }

        private void FrmMain_Load(object sender, EventArgs e)
        {

        }

        private void BtnEnviarEmail_Click(object sender, EventArgs e)
        {
            int i = 0;
            Int32.TryParse(boleto.TotalTitulosLista(), out i);

            if (i == 0) return;

            try
            {
                boleto.EnviarEmail(txtEmail.Text,
                    "Teste envio Boleto",
                    "Boleto em anexo", "");
                rtbRespostas.AppendLine("e-mail enviado!");

            } catch (Exception ex) {
                rtbRespostas.AppendLine(ex.Message);
            }

        }

        private void BtnListaBancos_Click(object sender, EventArgs e)
        {
            var ret = boleto.ListaBancos();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnCodigoBarras_Click(object sender, EventArgs e)
        {
            var ret = boleto.RetornaCodigoBarras(0);
            rtbRespostas.AppendLine(ret);
        }

        private void BtnLerRetorno_Click(object sender, EventArgs e)
        {
            boleto.LerRetorno(txtDirRetorno.Text, txtNomeRetorno.Text);
            rtbRespostas.AppendLine("Retorno Gerado.");
        }
    }
}