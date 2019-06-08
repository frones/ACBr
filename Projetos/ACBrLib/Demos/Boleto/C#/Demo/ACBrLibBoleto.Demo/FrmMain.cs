using System;
using System.Drawing.Printing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using ACBrLib;

namespace ACBrLibBoleto.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            var ret = ACBrBoleto.Boleto_Inicializar("".ToUTF8(), "".ToUTF8());
            ACBrBoleto.CheckResult(ret);
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Finalizando a dll
            var ret = ACBrBoleto.Boleto_Finalizar();
            ACBrBoleto.CheckResult(ret);
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cmbImpressora.Items.Add(printer);
            }

            if (PrinterSettings.InstalledPrinters.Count > 0)
                cmbImpressora.SelectedIndex = 0;

            // Altera as config de log
            var ret = ACBrBoleto.Boleto_ConfigGravarValor("Principal".ToUTF8(), "LogNivel".ToUTF8(), "4".ToUTF8());
            ACBrBoleto.CheckResult(ret);

            var logPath = Path.Combine(Application.StartupPath, "Docs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Principal".ToUTF8(), "LogPath".ToUTF8(), logPath.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravar("ACBrLib.ini".ToUTF8());
            ACBrBoleto.CheckResult(ret);

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void LoadConfig()
        {
            ACBrBoleto.Boleto_ConfigLer("".ToUTF8());

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "Layout".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbModeloImpressao.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "MostrarPreview".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            chkPreview.Checked = Convert.ToBoolean(Convert.ToInt32(buffer.FromUTF8()));

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "MostrarProgresso".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            chkProgresso.Checked = Convert.ToBoolean(Convert.ToInt32(buffer.FromUTF8()));

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "MostrarSetup".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            chkSetup.Checked = Convert.ToBoolean(Convert.ToInt32(buffer.FromUTF8()));

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "NomeArquivo".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNomeArquivo.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "NumeroCopias".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            nudCopias.Value = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "PrinterName".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbImpressora.SelectedItem = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoFCFortesConfig".ToUTF8(), "DirLogo".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtDirLogo.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoBancoConfig".ToUTF8(), "TipoCobranca".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbBanco.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Agencia".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtAgencia.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "AgenciaDigito".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtDigAgencia.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "AgenciaDigito".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtDigAgencia.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Conta".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtConta.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "ContaDigito".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtDigConta.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "CodigoTransmissao".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtCodTransmissao.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Convenio".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtConvenio.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Modalidade".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtModalidade.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "CodigoCedente".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtCodCedente.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "ResponEmissao".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbRespEmissao.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Bairro".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtBairro.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "CEP".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtCEP.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Cidade".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtCidade.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "CNPJCPF".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtCNPJCPF.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Complemento".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtComplemento.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Complemento".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtComplemento.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Logradouro".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtLogradouro.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Nome".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNomeRes.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "NumeroRes".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNumeroRes.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Telefone".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtTelefone.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "Telefone".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtTelefone.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "UF".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbUF.SelectedItem = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "TipoCarteira".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbTipoCarteira.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "TipoDocumento".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbTipoDocumento.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoCedenteConfig".ToUTF8(), "TipoInscricao".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbTipoInscricao.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "DirArqRemessa".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtDirRemessa.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "DirArqRetorno".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtDirRetorno.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "LayoutRemessa".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            cmbLayoutCNAB.SelectedIndex = Convert.ToInt32(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "NomeArqRemessa".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNomeRemessa.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "NomeArqRetorno".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNomeRetorno.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "LeCedenteRetorno".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            ckbCedenteRetorno.Checked = Convert.ToBoolean(Convert.ToInt32(buffer.FromUTF8()));

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("BoletoDiretorioConfig".ToUTF8(), "NomeArqRetorno".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNomeRetorno.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "Nome".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtNome.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "Conta".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtEmail.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "Usuario".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtUsuario.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "Senha".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtSenha.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "Servidor".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            txtHost.Text = buffer.FromUTF8();

            bufferLen = 256;
            buffer.Clear();
            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "Porta".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            nudPorta.Value = int.Parse(buffer.FromUTF8());

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "SSL".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            ckbSSL.Checked = Convert.ToBoolean(int.Parse(buffer.ToString()));

            bufferLen = 256;
            buffer.Clear();

            ret = ACBrBoleto.Boleto_ConfigLerValor("Email".ToUTF8(), "TLS".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            ckbTLS.Checked = Convert.ToBoolean(int.Parse(buffer.ToString()));
        }

        private void SaveConfig()
        {
            var ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "Layout".ToUTF8(), cmbModeloImpressao.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "MostrarPreview".ToUTF8(), (chkPreview.Checked ? "1" : "0").ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "MostrarProgresso".ToUTF8(), (chkProgresso.Checked ? "1" : "0").ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "MostrarSetup".ToUTF8(), (chkSetup.Checked ? "1" : "0").ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "NomeArquivo".ToUTF8(), txtNomeArquivo.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "NumeroCopias".ToUTF8(), nudCopias.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "PrinterName".ToUTF8(), cmbImpressora.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "DirLogo".ToUTF8(), txtDirLogo.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig".ToUTF8(), "TipoCobranca".ToUTF8(), cmbBanco.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Agencia".ToUTF8(), txtAgencia.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "AgenciaDigito".ToUTF8(), txtDigAgencia.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Conta".ToUTF8(), txtConta.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "ContaDigito".ToUTF8(), txtDigConta.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "CodigoTransmissao".ToUTF8(), txtCodTransmissao.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Convenio".ToUTF8(), txtConvenio.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Modalidade".ToUTF8(), txtModalidade.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "CodigoCedente".ToUTF8(), txtCodCedente.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "ResponEmissao".ToUTF8(), cmbRespEmissao.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Bairro".ToUTF8(), txtBairro.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "CEP".ToUTF8(), txtCEP.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Cidade".ToUTF8(), txtCidade.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "CNPJCPF".ToUTF8(), txtCNPJCPF.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Complemento".ToUTF8(), txtComplemento.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Logradouro".ToUTF8(), txtLogradouro.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Nome".ToUTF8(), txtNomeRes.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "NumeroRes".ToUTF8(), txtNumeroRes.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "Telefone".ToUTF8(), txtTelefone.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "UF".ToUTF8(), cmbUF.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "TipoCarteira".ToUTF8(), cmbTipoCarteira.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "TipoDocumento".ToUTF8(), cmbTipoDocumento.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoCedenteConfig".ToUTF8(), "TipoInscricao".ToUTF8(), cmbTipoInscricao.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoDiretorioConfig".ToUTF8(), "DirArqRemessa".ToUTF8(), txtDirRemessa.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoDiretorioConfig".ToUTF8(), "DirArqRetorno".ToUTF8(), txtDirRetorno.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoDiretorioConfig".ToUTF8(), "LayoutRemessa".ToUTF8(), cmbLayoutCNAB.SelectedIndex.ToString().ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoDiretorioConfig".ToUTF8(), "NomeArqRemessa".ToUTF8(), txtNomeRemessa.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoDiretorioConfig".ToUTF8(), "NomeArqRetorno".ToUTF8(), txtNomeRetorno.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("BoletoDiretorioConfig".ToUTF8(), "LeCedenteRetorno".ToUTF8(), (ckbCedenteRetorno.Checked ? "1" : "0").ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "Nome".ToUTF8(), txtNome.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "Conta".ToUTF8(), txtEmail.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "Usuario".ToUTF8(), txtUsuario.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "Senha".ToUTF8(), txtSenha.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "Servidor".ToUTF8(), txtHost.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "Porta".ToUTF8(), nudPorta.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "SSL".ToUTF8(), (ckbSSL.Checked ? "1" : "0").ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravarValor("Email".ToUTF8(), "TLS".ToUTF8(), (ckbTLS.Checked ? "1" : "0").ToUTF8());
            ACBrBoleto.CheckResult(ret);

            ret = ACBrBoleto.Boleto_ConfigGravar("ACBrLib.ini".ToUTF8());
            ACBrBoleto.CheckResult(ret);
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

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrBoleto.Boleto_IncluirTitulos(iniPath.ToUTF8(), "".ToUTF8(), buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            if (bufferLen > 256)
            {
                buffer.Clear();
                buffer.Capacity = bufferLen;

                ret = ACBrBoleto.Boleto_UltimoRetorno(buffer, ref bufferLen);
                ACBrBoleto.CheckResult(ret);
            }

            rtbRespostas.AppendLine(buffer);
        }

        private void BtnImprimir_Click(object sender, EventArgs e)
        {
            var ret = ACBrBoleto.Boleto_Imprimir("".ToUTF8());
            ACBrBoleto.CheckResult(ret);

            rtbRespostas.AppendLine("Boletos impressos.");
        }

        private void BtnGerarRemessa_Click(object sender, EventArgs e)
        {
            var ret = ACBrBoleto.Boleto_GerarRemessa(txtDirRemessa.Text.ToUTF8(), 1, txtNomeRemessa.Text.ToUTF8());
            ACBrBoleto.CheckResult(ret);

            rtbRespostas.AppendLine("Remessa Gerada.");
        }

        private void BtnTotalTitulo_Click(object sender, EventArgs e)
        {
            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrBoleto.Boleto_TotalTitulosLista(buffer, ref bufferLen);
            ACBrBoleto.CheckResult(ret);

            if (bufferLen > 256)
            {
                buffer.Clear();
                buffer.Capacity = bufferLen;

                ret = ACBrBoleto.Boleto_UltimoRetorno(buffer, ref bufferLen);
                ACBrBoleto.CheckResult(ret);
            }

            rtbRespostas.AppendLine(buffer);
        }

        private void BtnLimparLista_Click(object sender, EventArgs e)
        {
            var ret = ACBrBoleto.Boleto_LimparLista();
            ACBrBoleto.CheckResult(ret);

            rtbRespostas.AppendLine("Lista Limpa.");
        }

        #endregion EventHandlers
    }
}