using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Drawing.Printing;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ACBrLibPosPrinter.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            var ret = ACBrPosPrinter.POS_Inicializar("ACBrLib.ini".ToUTF8(), "".ToUTF8());
            ACBrPosPrinter.CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cbbModelo.EnumDataSource<ACBrPosPrinterModelo>();
            cbbPaginaCodigo.EnumDataSource<PosPaginaCodigo>();
            cbbPortas.Items.AddRange(SerialPort.GetPortNames());

            cbbPortas.Items.Add("LPT1");
            cbbPortas.Items.Add("LPT2");
            cbbPortas.Items.Add(@"\\localhost\Epson");
            cbbPortas.Items.Add(@"c:\temp\ecf.txt");

            cbbPortas.SelectedIndex = cbbPortas.Items.Count - 1;

            cbbPortas.Items.Add("TCP:192.168.0.31:9100");

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cbbPortas.Items.Add($"RAW:{printer}");
            }

            // Altera as config de log
            var ret = ACBrPosPrinter.POS_ConfigGravarValor("Principal".ToUTF8(), "LogNivel".ToUTF8(), "4".ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            var logPath = Path.Combine(Application.StartupPath, "Docs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("Principal".ToUTF8(), "LogPath".ToUTF8(), logPath.ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravar("ACBrLib.ini".ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            LoadConfig();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll da memoria
            ACBrPosPrinter.POS_Finalizar();
        }

        private void btnAtivar_Click(object sender, EventArgs e)
        {
            ToogleActivate();
        }

        private void btnArqLog_Click(object sender, EventArgs e)
        {
            txtArqLog.Text = Helpers.OpenFile("Arquivo Log (*.log)|*.log|Todo os Arquivos (*.*)|*.*", checkFileExists: false);
        }

        private void btnAddTags_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("</linha_dupla>");
            txtImpressao.AppendLine($"FONTE NORMAL: {(int)nudColunas.Value} Colunas");
            txtImpressao.AppendLine("</c><n>FONTE NEGRITO</N>");
            txtImpressao.AppendLine("<in>FONTE INVERTIDA</in>");
            txtImpressao.AppendLine("<S>FONTE SUBLINHADA</s>");
            txtImpressao.AppendLine("<i>FONTE ITALICO</i>");
            txtImpressao.AppendLine("FONTE NORMAL");
            txtImpressao.AppendLine("</linha_simples>");
            txtImpressao.AppendLine("<n>LIGA NEGRITO");
            txtImpressao.AppendLine("<i>LIGA ITALICO");
            txtImpressao.AppendLine("<S>LIGA SUBLINHADA");
            txtImpressao.AppendLine("<c>LIGA CONDENSADA");
            txtImpressao.AppendLine("<e>LIGA EXPANDIDA");
            txtImpressao.AppendLine("<a>LIGA ALTURA DUPLA");
            txtImpressao.AppendLine("</fn>FONTE NORMAL");
            txtImpressao.AppendLine("</linha_simples>");
            txtImpressao.AppendLine("<e><n>NEGRITO E EXPANDIDA</n></e>");
            txtImpressao.AppendLine("<c><n>NEGRITO E CONDENSADA</n></c>");
            txtImpressao.AppendLine("<e><a>EXPANDIDA E ALT.DUPLA</a></e>");
            txtImpressao.AppendLine("</fn>FONTE NORMAL");
            txtImpressao.AppendLine("<in><e>INVERTIDA E EXPANDIDA</e></in>");
            txtImpressao.AppendLine("<in><c>INVERTIDA E CONDENSADA</c></in>");
            txtImpressao.AppendLine("<in><a>INVERTIDA E ALT.DUPLA</a></in>");
            txtImpressao.AppendLine("</fn>FONTE NORMAL");
            txtImpressao.AppendLine("</linha_simples>");
            txtImpressao.AppendLine("</FB>FONTE TIPO B");
            txtImpressao.AppendLine("<n>FONTE NEGRITO</N>");
            txtImpressao.AppendLine("<e>FONTE EXPANDIDA</e>");
            txtImpressao.AppendLine("<a>FONTE ALT.DUPLA</a>");
            txtImpressao.AppendLine("<in>FONTE INVERTIDA</in>");
            txtImpressao.AppendLine("<S>FONTE SUBLINHADA</s>");
            txtImpressao.AppendLine("<i>FONTE ITALICO</i>");
            txtImpressao.AppendLine("</FA>FONTE TIPO A");
            txtImpressao.AppendLine("</FN>FONTE NORMAL");
            txtImpressao.AppendLine("</corte_total>");

            txtImpressao.AppendLine("<c>CODE128C: 35150711111111111111591234567890001135408700</c>");
            txtImpressao.AppendLine("<code128c>35150711111111111111591234567890001135408700</code128c>");

            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("UPCA: 12345678901");
            txtImpressao.AppendLine("<upca>12345678901</upca>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("CODABAR: A123456789012345A");
            txtImpressao.AppendLine("<codabar>A123456789012345A</codabar>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("MSI: 1234567890");
            txtImpressao.AppendLine("<msi>1234567890</msi>");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnLimpar_Click(object sender, EventArgs e)
        {
            txtImpressao.Clear();
        }

        private void btnImprimir_Click(object sender, EventArgs e)
        {
            var ret = ACBrPosPrinter.POS_Imprimir(txtImpressao.Text.ToUTF8(), false, true, true, 1);
            ACBrPosPrinter.CheckResult(ret);
        }

        #endregion EventHandlers

        private void LoadConfig()
        {
            ACBrPosPrinter.POS_ConfigLer("ACBrLib.ini");

            var bufferLen = 256;
            var pValue = new StringBuilder(bufferLen);

            var ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "Modelo".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbbModelo.SetSelectedValue((ACBrPosPrinterModelo)Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "Porta".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbbPortas.SelectedItem = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "ColunasFonteNormal".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            nudColunas.Value = Convert.ToInt32(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "EspacoEntreLinhas".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            nudEspacos.Value = Convert.ToInt32(pValue.FromUTF8());

            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "LinhasBuffer".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            nudBuffer.Value = Convert.ToInt32(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "LinhasEntreCupons".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            nudLinhasPular.Value = Convert.ToInt32(pValue.FromUTF8());

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "ControlePorta".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbxControlePorta.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "CortaPapel".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbxCortarPapel.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "TraduzirTags".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbxTraduzirTags.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "IgnorarTags".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbxIgnorarTags.Checked = Convert.ToBoolean(Convert.ToInt32(pValue.FromUTF8()));

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "IgnorarTags".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            txtArqLog.Text = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();
            ret = ACBrPosPrinter.POS_ConfigLerValor("PosPrinter".ToUTF8(), "PaginaDeCodigo".ToUTF8(), pValue, ref bufferLen);
            ACBrPosPrinter.CheckResult(ret);

            cbbPaginaCodigo.SetSelectedValue((PosPaginaCodigo)Convert.ToInt32(pValue.FromUTF8()));
        }

        private void SaveConfig()
        {
            var modelo = (int)cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>();
            var ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "Modelo".ToUTF8(), modelo.ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "Porta".ToUTF8(), cbbPortas.SelectedItem.ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "ColunasFonteNormal".ToUTF8(), ((int)nudColunas.Value).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "EspacoEntreLinhas".ToUTF8(), ((int)nudEspacos.Value).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "LinhasBuffer".ToUTF8(), ((int)nudBuffer.Value).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "LinhasEntreCupons".ToUTF8(), ((int)nudLinhasPular.Value).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "ControlePorta".ToUTF8(), Convert.ToInt32(cbxControlePorta.Checked).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "CortaPapel".ToUTF8(), Convert.ToInt32(cbxCortarPapel.Checked).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "TraduzirTags".ToUTF8(), Convert.ToInt32(cbxTraduzirTags.Checked).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "IgnorarTags".ToUTF8(), Convert.ToInt32(cbxIgnorarTags.Checked).ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "ArqLog".ToUTF8(), txtArqLog.Text.ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            var paginaCodigo = (int)cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>();
            ret = ACBrPosPrinter.POS_ConfigGravarValor("PosPrinter".ToUTF8(), "PaginaDeCodigo".ToUTF8(), paginaCodigo.ToString().ToUTF8());
            ACBrPosPrinter.CheckResult(ret);

            ret = ACBrPosPrinter.POS_ConfigGravar("ACBrLib.ini".ToUTF8());
            ACBrPosPrinter.CheckResult(ret);
        }

        private void ToogleActivate()
        {
            if (btnAtivar.Text == @"Ativar")
            {
                SaveConfig();
                var ret = ACBrPosPrinter.POS_Ativar();
                ACBrPosPrinter.CheckResult(ret);
                btnAtivar.Text = @"Desativar";
            }
            else
            {
                var ret = ACBrPosPrinter.POS_Desativar();
                ACBrPosPrinter.CheckResult(ret);
                btnAtivar.Text = @"Ativar";
            }
        }

        #endregion Methods
    }
}