using System;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PosPrinter;
using ACBrLib.Core.Sat;

namespace ACBrLib.Sat.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrSat acbrSat;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            acbrSat = new ACBrSat();
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

            cmbModeloSat.EnumDataSource(SATModelo.satNenhum);
            cmbImpressao.EnumDataSource(TipoRelatorioBobina.tpFortes);
            cbbModelo.EnumDataSource(ACBrPosPrinterModelo.Texto);
            cbbPaginaCodigo.EnumDataSource(PosPaginaCodigo.pc850);

            // Altera as config de log
            acbrSat.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", NivelLog.logParanoico);
            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            acbrSat.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
            acbrSat.ConfigGravar();

            LoadConfig();
        }

        private void FrmMain_FormClosed(object sender, FormClosedEventArgs e)
        {
            // Finalizando a dll
            acbrSat.Dispose();
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

                acbrSat.Inicializar();

                btnIniDesini.Text = "Desinicializar";
            }
            else
            {
                acbrSat.DesInicializar();
                btnIniDesini.Text = "Inicializar";
            }
        }

        private void btnConsultarSAT_Click(object sender, EventArgs e)
        {
            var ret = acbrSat.ConsultarSAT();
            rtbRespostas.AppendLine(ret);
        }

        private void btnConsultarStatus_Click(object sender, EventArgs e)
        {
            var ret = acbrSat.ConsultarStatusOperacional();
            rtbRespostas.AppendLine(ret);
        }

        private void btnCriarCFe_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Arquivo Ini CFe (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = acbrSat.CriarCFe(iniPath);
            rtbRespostas.AppendLine(ret);
        }

        private void btnCriarEnviarCFe_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Arquivo Ini CFe (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = acbrSat.CriarEnviarCFe(iniPath);
            rtbRespostas.AppendLine(ret);
        }

        private void btnEnviarCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var ret = acbrSat.EnviarCFe(xmlPath);
            rtbRespostas.AppendLine(ret);
        }

        private void btnCancelarCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var ret = acbrSat.CancelarCFe(xmlPath);
            rtbRespostas.AppendLine(ret);
        }

        private void btnImprimirCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            acbrSat.ImprimirExtratoVenda(xmlPath);
            rtbRespostas.AppendLine("Impressão efetuada com sucesso.");
        }

        private void btnImprimiCFeRed_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            acbrSat.ImprimirExtratoResumido(xmlPath);
            rtbRespostas.AppendLine("Impressão efetuada com sucesso.");
        }

        private void btnImprimirPDFCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            acbrSat.ImprimirExtratoVenda(xmlPath, cbbImpressora.Text);
            rtbRespostas.AppendLine("Impressão efetuada com sucesso.");
        }

        private void btnImpMFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var impressao = acbrSat.GerarImpressaoFiscalMFe(xmlPath);
            rtbRespostas.AppendLine(impressao);
        }

        private void btnEmail_Click(object sender, EventArgs e)
        {
        }

        private void LoadConfig()
        {
            acbrSat.ConfigLer();

            txtDllPath.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.SAT, "NomeDLL");
            cmbModeloSat.SetSelectedValue(acbrSat.ConfigLerValor<SATModelo>(ACBrSessao.SAT, "Modelo"));
            txtAtivacao.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.SAT, "CodigoDeAtivacao");
            nunVersaoCFe.Value = acbrSat.ConfigLerValor<decimal>(ACBrSessao.SATConfig, "infCFe_versaoDadosEnt");
            nunPaginaCodigo.Value = acbrSat.ConfigLerValor<decimal>(ACBrSessao.SATConfig, "PaginaDeCodigo");
            txtSignAc.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.SAT, "SignAC");
            chkSaveCFe.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SalvarCFe");
            chkSaveEnvio.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SalvarEnvio");
            chkSaveCFeCanc.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SalvarCFeCanc");
            chkSepararCNPJ.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SepararPorCNPJ");
            chkSepararData.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SepararPorDia");

            //Extrato
            cmbImpressao.SetSelectedValue(acbrSat.ConfigLerValor<TipoRelatorioBobina>(ACBrSessao.Extrato, "Tipo"));
            nudCopias.Value = acbrSat.ConfigLerValor<int>(ACBrSessao.Extrato, "Copias");
            txtSoftwareHouse.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Sistema, "Nome");
            cbbImpressora.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Extrato, "Impressora");
            txtSite.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Emissor, "WebSite");
            chkPreview.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.Extrato, "MostraPreview");
            chkSetup.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.Extrato, "MostraSetup");
            chkUsaCodigoEanImpressao.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.Extrato, "ImprimeCodigoEan");
            chkImprimeEmUmaLinha.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.Extrato, "ImprimeEmUmaLinha");

            //PosPrinter
            cbbModelo.SetSelectedValue(acbrSat.ConfigLerValor<ACBrPosPrinterModelo>(ACBrSessao.PosPrinter, "Modelo"));
            cbbPortas.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.PosPrinter, "Porta");
            cbbPaginaCodigo.SetSelectedValue(acbrSat.ConfigLerValor<PosPaginaCodigo>(ACBrSessao.PosPrinter, "PaginaDeCodigo"));
            nudColunas.Value = acbrSat.ConfigLerValor<int>(ACBrSessao.PosPrinter, "ColunasFonteNormal");
            nudEspacos.Value = acbrSat.ConfigLerValor<int>(ACBrSessao.PosPrinter, "EspacoEntreLinhas");
            nudBuffer.Value = acbrSat.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasBuffer");
            nudLinhasPular.Value = acbrSat.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasEntreCupons");
            cbxControlePorta.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "ControlePorta");
            cbxCortarPapel.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "CortaPapel");
            cbxTraduzirTags.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "TraduzirTags");
            cbxIgnorarTags.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "IgnorarTags");

            //Mail
            txtNome.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = acbrSat.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = acbrSat.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = acbrSat.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        private void SaveConfig()
        {
            acbrSat.ConfigGravarValor(ACBrSessao.SAT, "NomeDLL", txtDllPath.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.SAT, "Modelo", cmbModeloSat.GetSelectedValue<SATModelo>());
            acbrSat.ConfigGravarValor(ACBrSessao.SAT, "CodigoDeAtivacao", txtAtivacao.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfig, "infCFe_versaoDadosEnt", nunVersaoCFe.Value);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfig, "PaginaDeCodigo", nunPaginaCodigo.Value);
            acbrSat.ConfigGravarValor(ACBrSessao.SAT, "SignAC", txtSignAc.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SalvarCFe", chkSaveCFe.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SalvarEnvio", chkSaveEnvio.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SalvarCFeCanc", chkSaveCFeCanc.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SepararPorCNPJ", chkSepararCNPJ.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SepararPorDia", chkSepararData.Checked);

            //Impressão
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "Tipo", cmbImpressao.GetSelectedValue<TipoRelatorioBobina>());
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "Copias", nudCopias.Value);
            acbrSat.ConfigGravarValor(ACBrSessao.Sistema, "Nome", txtSoftwareHouse.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "Impressora", cbbImpressora.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Emissor, "WebSite", txtSite.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "MostraPreview", chkPreview.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "MostraSetup", chkSetup.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "ImprimeCodigoEan", chkUsaCodigoEanImpressao.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.Extrato, "ImprimeEmUmaLinha", chkImprimeEmUmaLinha.Checked);

            //PosPrinter
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "Modelo", cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "Porta", cbbPortas.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "ColunasFonteNormal", ((int)nudColunas.Value).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "EspacoEntreLinhas", ((int)nudEspacos.Value).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasBuffer", ((int)nudBuffer.Value).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasEntreCupons", ((int)nudLinhasPular.Value).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "ControlePorta", Convert.ToInt32(cbxControlePorta.Checked).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "CortaPapel", Convert.ToInt32(cbxCortarPapel.Checked).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "TraduzirTags", Convert.ToInt32(cbxTraduzirTags.Checked).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "IgnorarTags", Convert.ToInt32(cbxIgnorarTags.Checked).ToString());
            acbrSat.ConfigGravarValor(ACBrSessao.PosPrinter, "PaginaDeCodigo", cbbPaginaCodigo.SelectedIndex.ToString());

            //Mail
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Value);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
            acbrSat.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);

            acbrSat.ConfigGravar("");
        }

        #endregion Methods
    }
}