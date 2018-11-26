using ACBrLibMail;
using ACBrLibPosPrinter;
using System;
using System.Drawing.Printing;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace ACBrLibSat.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            var ret = ACBrSat.SAT_Inicializar("".ToUTF8(), "".ToUTF8());
            ACBrSat.CheckResult(ret);
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cbbPortas.Items.Add("LPT1");
            cbbPortas.Items.Add("LPT2");
            cbbPortas.Items.Add(@"\\localhost\Epson");
            cbbPortas.Items.Add(@"c:\temp\posprinter.txt");

            cbbPortas.SelectedIndex = cbbPortas.Items.Count - 1;

            cbbPortas.Items.Add("TCP:192.168.0.31:9100");

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cbbImpressora.Items.Add(printer);
                cbbPortas.Items.Add($"RAW:{printer}");
            }

            // Altera as config de log
            var ret = ACBrSat.SAT_ConfigGravarValor("Principal".ToUTF8(), "LogNivel".ToUTF8(), "4".ToUTF8());
            ACBrSat.CheckResult(ret);

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ret = ACBrSat.SAT_ConfigGravarValor("Principal".ToUTF8(), "LogPath".ToUTF8(), logPath.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravar("ACBrLib.ini".ToUTF8());
            ACBrSat.CheckResult(ret);

            LoadConfig();
        }

        private void FrmMain_FormClosed(object sender, FormClosedEventArgs e)
        {
            // Finalizando a dll
            var ret = ACBrSat.SAT_Finalizar();
            ACBrSat.CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        private void btnSelDll_Click(object sender, EventArgs e)
        {
            txtDllPath.Text = Helpers.OpenFile("Biblioteca SAT (*.dll)|*.dll|Todo os Arquivos (*.*)|*.*");
        }

        private void btnIniDesini_Click(object sender, EventArgs e)
        {
            if (btnIniDesini.Text == "Inicializar")
            {
                SaveConfig();

                var ret = ACBrSat.SAT_InicializarSAT();
                ACBrSat.CheckResult(ret);
                btnIniDesini.Text = "Desinicializar";
            }
            else
            {
                var ret = ACBrSat.SAT_DesInicializar();
                ACBrSat.CheckResult(ret);
                btnIniDesini.Text = "Inicializar";
            }
        }

        private void btnCriarCFe_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Arquivo Ini CFe (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrSat.SAT_CriarCFe(iniPath.ToUTF8(), buffer, ref bufferLen);
            ACBrSat.CheckResult(ret);

            if (bufferLen > 256)
            {
                buffer.Clear();
                buffer.Capacity = bufferLen;

                ret = ACBrSat.SAT_UltimoRetorno(buffer, ref bufferLen);
                ACBrSat.CheckResult(ret);
            }

            rtbRespostas.AppendText(buffer.ToString());
        }

        private void btnCriarEnviarCFe_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Arquivo Ini CFe (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrSat.SAT_CriarEnviarCFe(iniPath.ToUTF8(), buffer, ref bufferLen);
            ACBrSat.CheckResult(ret);

            if (bufferLen > 256)
            {
                buffer.Clear();
                buffer.Capacity = bufferLen;

                ret = ACBrSat.SAT_UltimoRetorno(buffer, ref bufferLen);
                ACBrSat.CheckResult(ret);
            }

            rtbRespostas.AppendText(buffer.ToString());
        }

        private void btnEnviarCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrSat.SAT_EnviarCFe(xmlPath.ToUTF8(), buffer, ref bufferLen);
            ACBrSat.CheckResult(ret);

            if (bufferLen > 256)
            {
                buffer.Clear();
                buffer.Capacity = bufferLen;

                ret = ACBrSat.SAT_UltimoRetorno(buffer, ref bufferLen);
                ACBrSat.CheckResult(ret);
            }

            rtbRespostas.AppendText(buffer.ToString());
        }

        private void btnImprimirCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var ret = ACBrSat.SAT_ImprimirExtratoVenda(xmlPath.ToUTF8(), "".ToUTF8());
            ACBrSat.CheckResult(ret);

            rtbRespostas.AppendText("Impressão efetuada com sucesso.");
        }

        private void btnImprimiCFeRed_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var ret = ACBrSat.SAT_ImprimirExtratoResumido(xmlPath.ToUTF8(), "".ToUTF8());
            ACBrSat.CheckResult(ret);

            rtbRespostas.AppendText("Impressão efetuada com sucesso.");
        }

        private void btnImprimirPDFCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var pdfFile = Helpers.SaveFile("Arquivo PDF (*.pdf)|*.pdf|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(pdfFile)) return;

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            var ret = ACBrSat.SAT_GerarPDFExtratoVenda(xmlPath.ToUTF8(), pdfFile.ToUTF8(), buffer, ref bufferLen);
            ACBrSat.CheckResult(ret);

            if (bufferLen > 256)
            {
                buffer.Clear();
                buffer.Capacity = bufferLen;

                ret = ACBrSat.SAT_UltimoRetorno(buffer, ref bufferLen);
                ACBrSat.CheckResult(ret);
            }

            rtbRespostas.AppendText(buffer.ToString());
        }

        private void LoadConfig()
        {
            var ret = ACBrSat.SAT_ConfigLer("".ToUTF8());
            ACBrSat.CheckResult(ret);

            var bufferLen = 256;
            var pValue = new StringBuilder(bufferLen);

            ret = ACBrSat.SAT_ConfigLerValor("SAT".ToUTF8(), "NomeDLL".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtDllPath.Text = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SAT".ToUTF8(), "Modelo".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cmbModeloSat.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SAT".ToUTF8(), "CodigoDeAtivacao".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtAtivacao.Text = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfig".ToUTF8(), "infCFe_versaoDadosEnt".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudVersaoCFe.Value = decimal.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfig".ToUTF8(), "PaginaDeCodigo".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudPaginaCodigo.Value = decimal.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SAT".ToUTF8(), "SignAC".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtSignAc.Text = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfigArquivos".ToUTF8(), "SalvarCFe".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkSaveCFe.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfigArquivos".ToUTF8(), "SalvarEnvio".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkSaveEnvio.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfigArquivos".ToUTF8(), "SalvarCFeCanc".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkSaveCFeCanc.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfigArquivos".ToUTF8(), "SepararPorCNPJ".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkSepararCNPJ.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("SATConfigArquivos".ToUTF8(), "SepararPorDia".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkSepararData.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            //Extrato
            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "Tipo".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cmbImpressao.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "NumCopias".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudCopias.Value = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "SoftwareHouse".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtSoftwareHouse.Text = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "PrinterName".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbbImpressora.Text = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "Site".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtSite.Text = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "MostrarPreview".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkPreview.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "MostrarSetup".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkSetup.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "UsaCodigoEanImpressao".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkUsaCodigoEanImpressao.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Extrato".ToUTF8(), "ImprimeEmUmaLinha".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            chkImprimeEmUmaLinha.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            //PosPrinter
            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "Modelo".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbbModelo.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "Porta".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbbPortas.SelectedItem = pValue.ToString();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "PaginaDeCodigo".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbbPaginaCodigo.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "ColunasFonteNormal".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudColunas.Value = Convert.ToInt32(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "EspacoEntreLinhas".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudEspacos.Value = Convert.ToInt32(pValue.FromUTF8());

            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "LinhasBuffer".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudBuffer.Value = Convert.ToInt32(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "LinhasEntreCupons".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudLinhasPular.Value = Convert.ToInt32(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "ControlePorta".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbxControlePorta.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "CortaPapel".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbxCortarPapel.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "TraduzirTags".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbxTraduzirTags.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("PosPrinter".ToUTF8(), "IgnorarTags".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            cbxIgnorarTags.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            //Mail
            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "Nome".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtNome.Text = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "Conta".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtEmail.Text = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "Usuario".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtUsuario.Text = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "Senha".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtSenha.Text = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "Servidor".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            txtHost.Text = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "Porta".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            nudPorta.Value = int.Parse(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "SSL".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            ckbSSL.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrSat.SAT_ConfigLerValor("Email".ToUTF8(), "TLS".ToUTF8(), pValue, ref bufferLen);
            ACBrSat.CheckResult(ret);

            ckbTLS.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));
        }

        private void SaveConfig()
        {
            var ret = ACBrSat.SAT_ConfigGravarValor("SAT".ToUTF8(), "NomeDLL".ToUTF8(), txtDllPath.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SAT".ToUTF8(), "Modelo".ToUTF8(), cmbModeloSat.SelectedIndex.ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SAT".ToUTF8(), "CodigoDeAtivacao".ToUTF8(), txtAtivacao.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfig".ToUTF8(), "infCFe_versaoDadosEnt".ToUTF8(), nudVersaoCFe.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfig".ToUTF8(), "PaginaDeCodigo".ToUTF8(), nudPaginaCodigo.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SAT".ToUTF8(), "SignAC".ToUTF8(), txtSignAc.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfigArquivos".ToUTF8(), "SalvarCFe".ToUTF8(), chkSaveCFe.Checked ? "1" : "0");
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfigArquivos".ToUTF8(), "SalvarEnvio".ToUTF8(), chkSaveEnvio.Checked ? "1" : "0");
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfigArquivos".ToUTF8(), "SalvarCFeCanc".ToUTF8(), chkSaveCFeCanc.Checked ? "1" : "0");
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfigArquivos".ToUTF8(), "SepararPorCNPJ".ToUTF8(), chkSepararCNPJ.Checked ? "1" : "0");
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("SATConfigArquivos".ToUTF8(), "SepararPorDia".ToUTF8(), chkSepararData.Checked ? "1" : "0");
            ACBrSat.CheckResult(ret);

            //Impressão
            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "Tipo".ToUTF8(), cmbImpressao.SelectedIndex.ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "NumCopias".ToUTF8(), nudCopias.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "SoftwareHouse".ToUTF8(), txtSoftwareHouse.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "PrinterName".ToUTF8(), cbbImpressora.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "Site".ToUTF8(), txtSite.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "MostrarPreview".ToUTF8(), (chkPreview.Checked ? "1" : "0").ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "MostrarSetup".ToUTF8(), (chkSetup.Checked ? "1" : "0").ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "UsaCodigoEanImpressao".ToUTF8(), (chkUsaCodigoEanImpressao.Checked ? "1" : "0").ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Extrato".ToUTF8(), "ImprimeEmUmaLinha".ToUTF8(), (chkImprimeEmUmaLinha.Checked ? "1" : "0").ToUTF8());
            ACBrSat.CheckResult(ret);

            //PosPrinter
            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "Modelo".ToUTF8(), cbbModelo.SelectedIndex.ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "Porta".ToUTF8(), cbbPortas.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "ColunasFonteNormal".ToUTF8(), ((int)nudColunas.Value).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "EspacoEntreLinhas".ToUTF8(), ((int)nudEspacos.Value).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "LinhasBuffer".ToUTF8(), ((int)nudBuffer.Value).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "LinhasEntreCupons".ToUTF8(), ((int)nudLinhasPular.Value).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "ControlePorta".ToUTF8(), Convert.ToInt32(cbxControlePorta.Checked).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "CortaPapel".ToUTF8(), Convert.ToInt32(cbxCortarPapel.Checked).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "TraduzirTags".ToUTF8(), Convert.ToInt32(cbxTraduzirTags.Checked).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "IgnorarTags".ToUTF8(), Convert.ToInt32(cbxIgnorarTags.Checked).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("PosPrinter".ToUTF8(), "PaginaDeCodigo".ToUTF8(), cbbPaginaCodigo.SelectedIndex.ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            //Mail
            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "Nome".ToUTF8(), txtNome.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "Conta".ToUTF8(), txtEmail.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "Usuario".ToUTF8(), txtUsuario.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "Senha".ToUTF8(), txtSenha.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "Servidor".ToUTF8(), txtHost.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "Porta".ToUTF8(), nudPorta.Text.ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "SSL".ToUTF8(), Convert.ToInt32(ckbSSL.Checked).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravarValor("Email".ToUTF8(), "TLS".ToUTF8(), Convert.ToInt32(ckbTLS.Checked).ToString().ToUTF8());
            ACBrSat.CheckResult(ret);

            ret = ACBrSat.SAT_ConfigGravar("".ToUTF8());
            ACBrSat.CheckResult(ret);
        }

        #endregion Methods
    }
}