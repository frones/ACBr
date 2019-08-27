using System;
using System.Drawing;
using System.Drawing.Printing;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.ETQ;
using ACBrLib.ETQ;

namespace ACBrLibETQ.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrETQ acbrEtq;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            acbrEtq = new ACBrETQ();
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

            comboBoxModelo.EnumDataSource(ETQModelo.etqNenhum);
            comboBoxDPI.EnumDataSource(ETQDPI.dpi300);
            comboBoxBackFeed.EnumDataSource(ETQBackFeed.bfNone);

            // Altera as config de log
            acbrEtq.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", "4");

            var logPath = Path.Combine(Application.StartupPath, "Docs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            acbrEtq.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);

            acbrEtq.ConfigGravar();
            LoadConfig();
        }

        private void buttonCarregarImagem_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("BMP MonoCromático|*.bmp|PCX|*.pcx|IMG|*.img");

            if (string.IsNullOrEmpty(file)) return;

            pictureBox.Image = Image.FromFile(file);

            acbrEtq.CarregarImagem(file, textBoxImagem.Text, false);
        }

        private void buttonEtqSimples_Click(object sender, EventArgs e)
        {
            SaveConfig();

            acbrEtq.Ativar();

            if ((new[] { 1, 2 }).Contains(comboBoxModelo.SelectedIndex))
            {
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirCaixa(13, 32, 56, 17, 1, 1);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "20,59");
            }
            else
            {
                acbrEtq.ImprimirCaixa(3, 3, 90, 5, 5, 0);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.or180, "S", 10, 10, 8, 3, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirCaixa(13, 32, 56, 17, 1, 1);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "20,59");
            }

            acbrEtq.Imprimir((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            acbrEtq.Desativar();
        }

        private void buttonEtq3Colunas_Click(object sender, EventArgs e)
        {
            SaveConfig();

            acbrEtq.Ativar();

            if ((new[] { 1, 2 }).Contains(comboBoxModelo.SelectedIndex))
            {
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 1, 2, 2, 3, "BISCOITO REC 33G");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 1, 1, 6, 3, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 3, "7896003701685", 10);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 1, 2, 2, 32, "BISCOITO RECH 33G");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 1, 1, 6, 32, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 32, "7896003701685", 10);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 1, 2, 2, 61, "BISCOITO RECH 33G");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 1, 1, 6, 61, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 61, "7896003701685", 10);
            }
            else
            {
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "0", 20, 30, 2, 3, "BISCOITO REC 33G");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "0", 20, 20, 6, 3, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 3, "7896003701685", 10);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "0", 20, 30, 2, 32, "BISCOITO RECH 33G");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "0", 20, 20, 6, 32, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 32, "7896003701685", 10);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "0", 20, 30, 2, 61, "BISCOITO RECH 33G");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "0", 20, 20, 6, 61, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 61, "7896003701685", 10);
            }

            acbrEtq.Imprimir((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            acbrEtq.Desativar();
        }

        private void buttonEtqBloco_Click(object sender, EventArgs e)
        {
            SaveConfig();

            acbrEtq.Ativar();

            if ((new[] { 1, 2 }).Contains(comboBoxModelo.SelectedIndex))
            {
                acbrEtq.IniciarEtiqueta();
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "20,59");
                acbrEtq.FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);

                acbrEtq.IniciarEtiqueta();
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "SABAO EM PO FLASH 1KG", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "ADVANCED - UNIDADE");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898903097042", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "3,18");
                acbrEtq.FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);

                acbrEtq.IniciarEtiqueta();
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "MACIO MATRIX FIX");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898237690230", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "8,60");
                acbrEtq.FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            }
            else
            {
                acbrEtq.IniciarEtiqueta();
                acbrEtq.ImprimirCaixa(3, 3, 90, 5, 5, 0);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "S", 10, 10, 8, 3, "CHOC BRANCO");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "20,59");
                acbrEtq.FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);

                acbrEtq.IniciarEtiqueta();
                acbrEtq.ImprimirCaixa(3, 3, 90, 5, 5, 0);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "SABAO EM PO FLASH 1KG", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "S", 10, 10, 8, 3, "ADVANCED - UNIDADE");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898903097042", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "3,18");
                acbrEtq.FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);

                acbrEtq.IniciarEtiqueta();
                acbrEtq.ImprimirCaixa(3, 3, 90, 5, 5, 0);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, true);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "S", 10, 10, 8, 3, "MACIO MATRIX FIX");
                acbrEtq.ImprimirBarras(ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898237690230", 10, ETQBarraExibeCodigo.becSIM);
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$");
                acbrEtq.ImprimirTexto(ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "8,60");
                acbrEtq.FinalizarEtiqueta((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            }

            acbrEtq.Imprimir(1, (int)numericUpDownAvancoEtq.Value);
            acbrEtq.Desativar();
        }

        private void buttonEtqImagem_Click(object sender, EventArgs e)
        {
            SaveConfig();

            acbrEtq.Ativar();
            acbrEtq.ImprimirImagem(1, 10, 10, textBoxImagem.Text);
            acbrEtq.Imprimir((int)numericUpDownCopias.Value, (int)numericUpDownAvancoEtq.Value);
            acbrEtq.Desativar();
        }

        private void LoadConfig()
        {
            acbrEtq.ConfigLer("");

            cbbPortas.SelectedItem = acbrEtq.ConfigLerValor<string>(ACBrSessao.ETQ, "Porta");
            comboBoxModelo.SetSelectedValue(acbrEtq.ConfigLerValor<ETQModelo>(ACBrSessao.ETQ, "Modelo"));
            numericUpDownTemperatura.Value = acbrEtq.ConfigLerValor<decimal>(ACBrSessao.ETQ, "Temperatura");
            comboBoxDPI.SetSelectedValue(acbrEtq.ConfigLerValor<ETQDPI>(ACBrSessao.ETQ, "DPI"));
            numericUpDownVelocidade.Value = acbrEtq.ConfigLerValor<decimal>(ACBrSessao.ETQ, "Velocidade");
            comboBoxBackFeed.SetSelectedValue(acbrEtq.ConfigLerValor<ETQBackFeed>(ACBrSessao.ETQ, "BackFeed"));
            numericUpDownAvancoEtq.Value = acbrEtq.ConfigLerValor<decimal>(ACBrSessao.ETQ, "Avanco");
            checkBoxLimparMemoria.Checked = acbrEtq.ConfigLerValor<bool>(ACBrSessao.ETQ, "LimparMemoria");
        }

        private void SaveConfig()
        {
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "Porta", cbbPortas.SelectedText);
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "Modelo", comboBoxModelo.GetSelectedValue<ETQModelo>());
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "Temperatura", numericUpDownTemperatura.Value);
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "DPI", comboBoxDPI.GetSelectedValue<ETQDPI>());
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "Velocidade", numericUpDownVelocidade.Value);
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "BackFeed", comboBoxBackFeed.GetSelectedValue<ETQBackFeed>());
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "Avanco", numericUpDownAvancoEtq.Value);
            acbrEtq.ConfigGravarValor(ACBrSessao.ETQ, "LimparMemoria", checkBoxLimparMemoria.Checked);
            acbrEtq.ConfigGravar("");
        }

        #endregion Methods
    }
}