using System;
using System.Drawing;
using System.Drawing.Printing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using ACBrLib;

namespace ACBrLibETQ.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            var ret = ACBrETQ.ETQ_Inicializar("".ToUTF8(), "".ToUTF8());
            ACBrETQ.CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cbbPortas.Items.Add("LPT1");
            cbbPortas.Items.Add("LPT2");
            cbbPortas.Items.Add(@"\\localhost\Epson");
            cbbPortas.Items.Add(@"c:\temp\etq.txt");

            cbbPortas.SelectedIndex = cbbPortas.Items.Count - 1;

            cbbPortas.Items.Add("TCP:192.168.0.31:9100");

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cbbPortas.Items.Add($"RAW:{printer}");
            }

            comboBoxModelo.Items.Add("etqNenhum");
            comboBoxModelo.Items.Add("etqPpla");
            comboBoxModelo.Items.Add("etqPplb");
            comboBoxModelo.Items.Add("etqZPLII");
            comboBoxModelo.Items.Add("etqEpl2");
            comboBoxModelo.SelectedIndex = 0;

            comboBoxDPI.Items.Add("dpi203");
            comboBoxDPI.Items.Add("dpi300");
            comboBoxDPI.Items.Add("dpi600");
            comboBoxDPI.SelectedIndex = 1;

            comboBoxBackFeed.Items.Add("bfNone");
            comboBoxBackFeed.Items.Add("bfOn");
            comboBoxBackFeed.Items.Add("bfOff");
            comboBoxBackFeed.SelectedIndex = 0;

            // Altera as config de log
            var ret = ACBrETQ.ETQ_ConfigGravarValor("Principal".ToUTF8(), "LogNivel".ToUTF8(), "4".ToUTF8());
            ACBrETQ.CheckResult(ret);

            var logPath = Path.Combine(Application.StartupPath, "Docs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ret = ACBrETQ.ETQ_ConfigGravarValor("Principal".ToUTF8(), "LogPath".ToUTF8(), logPath.ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravar("ACBrLib.ini".ToUTF8());
            ACBrETQ.CheckResult(ret);

            LoadConfig();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Finalizando a dll
            var ret = ACBrETQ.ETQ_Finalizar();
            ACBrETQ.CheckResult(ret);
        }

        private void buttonCarregarImagem_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("BMP MonoCromático|*.bmp|PCX|*.pcx|IMG|*.img");

            if (string.IsNullOrEmpty(file)) return;

            pictureBox.Image = Image.FromFile(file);

            var ret = ACBrETQ.ETQ_CarregarImagem(file, textBoxImagem.Text, false);
            ACBrETQ.CheckResult(ret);
        }

        private void buttonEtqSimples_Click(object sender, EventArgs e)
        {
            SaveConfig();

            var ret = ACBrETQ.ETQ_Ativar();
            ACBrETQ.CheckResult(ret);

            if ((new[] { 1, 2 }).Contains(comboBoxModelo.SelectedIndex))
            {
                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirCaixa(13, 32, 56, 17, 1, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "20,59", 0, false);
                ACBrETQ.CheckResult(ret);
            }
            else
            {
                ret = ACBrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(2, "S", 10, 10, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirCaixa(13, 32, 56, 17, 1, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 55, 100, 15, 50, "20,59", 0, false);
                ACBrETQ.CheckResult(ret);
            }

            ret = ACBrETQ.ETQ_Imprimir((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_Desativar();
            ACBrETQ.CheckResult(ret);
        }

        private void buttonEtq3Colunas_Click(object sender, EventArgs e)
        {
            SaveConfig();

            var ret = ACBrETQ.ETQ_Ativar();
            ACBrETQ.CheckResult(ret);

            if ((new[] { 1, 2 }).Contains(comboBoxModelo.SelectedIndex))
            {
                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 3, "BISCOITO REC 33G", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685", 10, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 32, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 32, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685", 10, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 61, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 61, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685", 10, 0);
                ACBrETQ.CheckResult(ret);
            }
            else
            {
                ret = ACBrETQ.ETQ_ImprimirTexto(0, "0", 20, 30, 2, 3, "BISCOITO REC 33G", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "0", 20, 20, 6, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685", 10, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "0", 20, 30, 2, 32, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "0", 20, 20, 6, 32, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685", 10, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "0", 20, 30, 2, 61, "BISCOITO RECH 33G", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "0", 20, 20, 6, 61, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685", 10, 0);
                ACBrETQ.CheckResult(ret);
            }

            ret = ACBrETQ.ETQ_Imprimir((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_Desativar();
            ACBrETQ.CheckResult(ret);
        }

        private void buttonEtqBloco_Click(object sender, EventArgs e)
        {
            SaveConfig();

            var ret = ACBrETQ.ETQ_Ativar();
            ACBrETQ.CheckResult(ret);

            if ((new[] { 1, 2 }).Contains(comboBoxModelo.SelectedIndex))
            {
                ret = ACBrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "20,59", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "SABAO EM PO FLASH 1KG", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "ADVANCED - UNIDADE", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898903097042", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "3,18", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "MACIO MATRIX FIX", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898237690230", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "8,60", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
                ACBrETQ.CheckResult(ret);
            }
            else
            {
                ret = ACBrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "S", 10, 10, 8, 3, "CHOC BRANCO", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 55, 100, 15, 50, "20,59", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "T", 10, 10, 3, 3, "SABAO EM PO FLASH 1KG", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "S", 10, 10, 8, 3, "ADVANCED - UNIDADE", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898903097042", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 55, 100, 15, 50, "3,18", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_IniciarEtiqueta();
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "T", 10, 10, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, true);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "S", 10, 10, 8, 3, "MACIO MATRIX FIX", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898237690230", 10, 1);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 40, 80, 18, 35, "R$", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_ImprimirTexto(0, "G", 55, 100, 15, 50, "8,60", 0, false);
                ACBrETQ.CheckResult(ret);

                ret = ACBrETQ.ETQ_FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
                ACBrETQ.CheckResult(ret);
            }

            ret = ACBrETQ.ETQ_Imprimir(1, (int)numericUpDownAvancoEtq.Value);
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_Desativar();
            ACBrETQ.CheckResult(ret);
        }

        private void buttonEtqImagem_Click(object sender, EventArgs e)
        {
            SaveConfig();

            var ret = ACBrETQ.ETQ_Ativar();
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ImprimirImagem(1, 10, 10, textBoxImagem.Text);
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_Imprimir((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_Desativar();
            ACBrETQ.CheckResult(ret);
        }

        private void LoadConfig()
        {
            ACBrETQ.ETQ_ConfigLer("".ToUTF8());

            var bufferLen = 256;
            var pValue = new StringBuilder(bufferLen);

            var ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "Porta".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            cbbPortas.SelectedItem = pValue.FromUTF8();

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "Modelo".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            comboBoxModelo.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "Temperatura".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            numericUpDownTemperatura.Value = decimal.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "DPI".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            comboBoxDPI.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "Velocidade".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            numericUpDownVelocidade.Value = decimal.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "BackFeed".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            comboBoxBackFeed.SelectedIndex = int.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "Avanco".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            numericUpDownAvancoEtq.Value = decimal.Parse(pValue.ToString());

            bufferLen = 256;
            pValue.Clear();

            ret = ACBrETQ.ETQ_ConfigLerValor("ETQ".ToUTF8(), "LimparMemoria".ToUTF8(), pValue, ref bufferLen);
            ACBrETQ.CheckResult(ret);

            checkBoxLimparMemoria.Checked = Convert.ToBoolean(int.Parse(pValue.ToString()));
        }

        private void SaveConfig()
        {
            var ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "Porta".ToUTF8(), cbbPortas.SelectedText.ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "Modelo".ToUTF8(), comboBoxModelo.SelectedIndex.ToString().ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "Temperatura".ToUTF8(), numericUpDownTemperatura.Text.ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "DPI".ToUTF8(), comboBoxDPI.SelectedIndex.ToString().ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "Velocidade".ToUTF8(), numericUpDownVelocidade.Text.ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "BackFeed".ToUTF8(), comboBoxBackFeed.SelectedIndex.ToString().ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "Avanco".ToUTF8(), numericUpDownAvancoEtq.Text.ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravarValor("ETQ".ToUTF8(), "LimparMemoria".ToUTF8(), (checkBoxLimparMemoria.Checked ? "1" : "0").ToUTF8());
            ACBrETQ.CheckResult(ret);

            ret = ACBrETQ.ETQ_ConfigGravar("".ToUTF8());
            ACBrETQ.CheckResult(ret);
        }

        #endregion Methods
    }
}