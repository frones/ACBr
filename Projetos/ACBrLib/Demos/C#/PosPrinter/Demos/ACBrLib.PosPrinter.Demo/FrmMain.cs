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
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.PosPrinter;
using ACBrLib.Core.Serial;

namespace ACBrLibPosPrinter.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrPosPrinter posPrinter;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            posPrinter = new ACBrPosPrinter();
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cbbModelo.EnumDataSource(ACBrPosPrinterModelo.ppTexto);
            cbbPaginaCodigo.EnumDataSource(PosPaginaCodigo.pc850);

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

            LoadConfig();

            // Altera as config de log
            posPrinter.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);
            posPrinter.ConfigGravarValor(ACBrSessao.Principal, "LogPath", Path.Combine(Application.StartupPath, "Docs"));
            posPrinter.ConfigGravar();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll
            posPrinter.Dispose();
        }

        private void btnAtivar_Click(object sender, EventArgs e)
        {
            ToogleActivate();
        }

        private void btnSerial_Click(object sender, EventArgs e)
        {
            using (var form = new FrmSerial())
            {
                form.Device = new ACBrDevice(posPrinter.ConfigLerValor<string>(ACBrSessao.PosPrinter, "Device"))
                {
                    Porta = (string)cbbPortas.SelectedItem
                };

                if (form.ShowDialog(this) != DialogResult.OK) return;

                cbbPortas.SelectedItem = form.Device.Porta;

                posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "Porta", form.Device.Porta);
                posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "TimeOut", form.Device.TimeOut);
                posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "Device", form.Device);
                posPrinter.ConfigGravar();
            }
        }

        private void btnArqLog_Click(object sender, EventArgs e)
        {
            txtArqLog.Text = Helpers.OpenFile("Arquivo Log (*.log)|*.log|Todo os Arquivos (*.*)|*.*", checkFileExists: false);
        }

        private void btnSenha_Click(object sender, EventArgs e)
        {
            posPrinter.Imprimir("<code93>1234" + 0x9 + "5678</code93></corte_total>");
        }

        private void btnLimpar_Click(object sender, EventArgs e)
        {
            txtImpressao.Clear();
        }

        private void btnImprimir_Click(object sender, EventArgs e)
        {
            posPrinter.Imprimir(txtImpressao.Text);
        }

        private void btnTagsFormatacao_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("</linha_dupla>");
            txtImpressao.AppendLine($"FONTE NORMAL: {nudColunas.Text} Colunas");
            //txtImpressao.AppendLine(LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", ACBrPosPrinter1.ColunasFonteNormal));
            //txtImpressao.AppendLine("<e>EXPANDIDO: " + IntToStr(ACBrPosPrinter1.ColunasFonteExpandida) + " Colunas");
            //txtImpressao.AppendLine(LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", ACBrPosPrinter1.ColunasFonteExpandida));
            //txtImpressao.AppendLine("</e><c>CONDENSADO: " + IntToStr(ACBrPosPrinter1.ColunasFonteCondensada) + " Colunas");
            //txtImpressao.AppendLine(LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", ACBrPosPrinter1.ColunasFonteCondensada));
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

        private void btnTagCodbar_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("<barra_mostrar>" + (cbxCodbarExibeNumeros.Checked ? "1" : "0") + "</barra_mostrar>");
            txtImpressao.AppendLine("<barra_largura>" + nudCodbarLargura + "</barra_largura>");
            txtImpressao.AppendLine("<barra_altura>" + nudCodbarAltura + "</barra_altura>");
            txtImpressao.AppendLine("</ce>");
            txtImpressao.AppendLine("</linha_dupla>");
            txtImpressao.AppendLine("EAN 8: 1234567");
            txtImpressao.AppendLine("<ean8>1234567</ean8>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("EAN13: 123456789012");
            txtImpressao.AppendLine("<ean13>123456789012</ean13>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("std25: 1234567890");
            txtImpressao.AppendLine("<std>1234567890</std>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("INT25: 1234567890");
            txtImpressao.AppendLine("<inter>1234567890</inter>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("CODE11: 1234567890");
            txtImpressao.AppendLine("<code11>1234567890</code11>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("CODE39: ABCDE12345");
            txtImpressao.AppendLine("<code39>ABCDE12345</code39>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("CODE93: ABC123abc");
            txtImpressao.AppendLine("<code93>ABC123abc</code93>");
            txtImpressao.AppendLine("</Linha_Simples>");
            txtImpressao.AppendLine("CODE128: $-=+ABC123abc");
            txtImpressao.AppendLine("<code128>$-=+ABC123abc</code128>");
            txtImpressao.AppendLine("CODE128C: 3515071111111111111159");
            txtImpressao.AppendLine("<code128c>3515071111111111111159</code128c>");
        }

        private void btnTagValidas_Click(object sender, EventArgs e)
        {
            txtImpressao.Clear();

            txtImpressao.AppendLine(posPrinter.RetornarTags());
            //posPrinter.ImprimirTags();
        }

        private void btnTagAlinhamento_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("</linha_dupla>");
            txtImpressao.AppendLine("TEXTO NORMAL");
            txtImpressao.AppendLine("</ae>ALINHADO A ESQUERDA");
            txtImpressao.AppendLine("1 2 3 TESTANDO");
            txtImpressao.AppendLine("<n>FONTE NEGRITO</n>");
            txtImpressao.AppendLine("<e>FONTE EXPANDIDA</e>");
            txtImpressao.AppendLine("<a>FONTE ALT.DUPLA</a>");
            txtImpressao.AppendLine("<c>FONTE CONDENSADA</c>");
            txtImpressao.AppendLine("<in>FONTE INVERTIDA</in>");
            txtImpressao.AppendLine("<s>FONTE SUBLINHADA</s>");
            txtImpressao.AppendLine("<i>FONTE ITALICO</i>");

            txtImpressao.AppendLine("</fn></ce>ALINHADO NO CENTRO");
            txtImpressao.AppendLine("1 2 3 TESTANDO");
            txtImpressao.AppendLine("<n>FONTE NEGRITO</n>");
            txtImpressao.AppendLine("<e>FONTE EXPANDIDA</e>");
            txtImpressao.AppendLine("<a>FONTE ALT.DUPLA</a>");
            txtImpressao.AppendLine("<c>FONTE CONDENSADA</c>");
            txtImpressao.AppendLine("<in>FONTE INVERTIDA</in>");
            txtImpressao.AppendLine("<S>FONTE SUBLINHADA</s>");
            txtImpressao.AppendLine("<i>FONTE ITALICO</i>");

            txtImpressao.AppendLine("</fn></ad>ALINHADO A DIREITA");
            txtImpressao.AppendLine("1 2 3 TESTANDO");
            txtImpressao.AppendLine("<n>FONTE NEGRITO</N>");
            txtImpressao.AppendLine("<e>FONTE EXPANDIDA</e>");
            txtImpressao.AppendLine("<a>FONTE ALT.DUPLA</a>");
            txtImpressao.AppendLine("<c>FONTE CONDENSADA</e>");
            txtImpressao.AppendLine("<in>FONTE INVERTIDA</in>");
            txtImpressao.AppendLine("<S>FONTE SUBLINHADA</s>");
            txtImpressao.AppendLine("<i>FONTE ITALICO</i>");

            txtImpressao.AppendLine("</ae></fn>TEXTO NORMAL");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnTagQRCode_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("</linha_dupla>");
            txtImpressao.AppendLine("<qrcode_tipo>" + nudQRTipo + "</qrcode_tipo>");
            txtImpressao.AppendLine("<qrcode_largura>" + nudQRLargura + "</qrcode_largura>");
            txtImpressao.AppendLine("<qrcode_error>" + nudQRErrorLevel + "</qrcode_error>");
            txtImpressao.AppendLine("<qrcode>http://projetoacbr.com.br</qrcode>");
            txtImpressao.AppendLine("</ce>");
            txtImpressao.AppendLine("<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>");
            txtImpressao.AppendLine("</ad>");
            txtImpressao.AppendLine("<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/questoes_importantes.html</qrcode>");
            txtImpressao.AppendLine("</ce>");
            txtImpressao.AppendLine("Exemplo de QRCode para NFCe");
            txtImpressao.AppendLine("<qrcode_error>0</qrcode_error><qrcode>https://www.homologacao.nfce.fazenda.sp.gov.br/NFCeConsultaPublica/Paginas/ConsultaQRCode.aspx?chNFe=35150805481336000137650220000000711000001960&nVersao=100&tpAmb=2&dhEmi=323031352D30382D31395432323A33333A32352D30333A3030&vNF=3.00&vICMS=0.12&digVal=776967396F2B665861706673396878776E64594C396F61654C35493D&cIdToken=000001&cHashQRCode=9BD312D558823E1EC68CEDB338A39B6150B0480E</qrcode>");
            txtImpressao.AppendLine("Exemplo de QRCode para SAT");
            txtImpressao.AppendLine("<qrcode_error>0</qrcode_error><qrcode>35150811111111111111591234567890001672668828|20150820201736|118.72|05481336000137|TCbeD81ePUpMvso4VjFqRTvs4ovqmR1ZG3bwSCumzHtW8bbMedVJjVnww103v3LxKfgckAyuizcR/9pXaKay6M4Gu8kyDef+6VH5qONIZV1cB+mFfXiaCgeZALuRDCH1PRyb6hoBeRUkUk6lOdXSczRW9Y83GJMXdOFroEbzFmpf4+WOhe2BZ3mEdXKKGMfl1EB0JWnAThkGT+1Er9Jh/3En5YI4hgQP3NC2BiJVJ6oCEbKb85s5915DSZAw4qB/MlESWViDsDVYEnS/FQgA2kP2A9pR4+agdHmgWiz30MJYqX5Ng9XEYvvOMzl1Y6+7/frzsocOxfuQyFsnfJzogw==</qrcode>");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnTagGaveta_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("Abertura da Gaveta padrão");
            txtImpressao.AppendLine("</abre_gaveta>");
            txtImpressao.AppendLine("");
            txtImpressao.AppendLine("");
            txtImpressao.AppendLine("Abertura da Gaveta específica");
            txtImpressao.AppendLine("<abre_gaveta>" + nudGVGaveta.Text + "</abre_gaveta>");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnTagInvalidas_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("<CE>*** TESTE DE TAGS INVÁLIDAS ***</CE>");
            txtImpressao.AppendLine("<ce> <>tags inválidas no texto\" > \">><<</CE>");
            txtImpressao.AppendLine("<AD><da><ec></</A Direita</ad>");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnTagsLogo_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("</ce>");
            txtImpressao.AppendLine("<logo_imprimir>" + (posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Logo, "IgnorarLogo") ? "0" : "1") + "</logo_imprimir>");
            txtImpressao.AppendLine("<logo_kc1>" + nudLogoKC1.Text + "</logo_kc1>");
            txtImpressao.AppendLine("<logo_kc2>" + nudLogoKC2.Text + "</logo_kc2>");
            txtImpressao.AppendLine("<logo_fatorx>" + nudLogoFatorX.Text + "</logo_fatorx>");
            txtImpressao.AppendLine("<logo_fatory>" + nudLogoFatorY.Text + "</logo_fatory>");
            txtImpressao.AppendLine("</logo>");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnLeituraInfo_Click(object sender, EventArgs e)
        {
            if (!btnSerial.Enabled)
                ToogleActivate();

            txtImpressao.AppendLine(posPrinter.LerInfoImpressora());
        }

        private void btnPaginaCodigo_Click(object sender, EventArgs e)
        {
            txtImpressao.AppendLine("</zera>");
            txtImpressao.AppendLine("</linha_dupla>");
            txtImpressao.AppendLine("ÁÉÍÓÚáéíóúçÇãõÃÕÊêÀà");
            txtImpressao.AppendLine("</corte_total>");
        }

        private void btnImpressaoLinha_Click(object sender, EventArgs e)
        {
            posPrinter.ImprimirLinha("</zera>");
            posPrinter.ImprimirLinha("</linha_dupla>");
            posPrinter.ImprimirLinha("FONTE NORMAL: " + nudColunas.Text + " Colunas");
            //posPrinter.ImprimirLinha(LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", posPrinter.ColunasFonteNormal));
            //posPrinter.ImprimirLinha("<e>EXPANDIDO: " + IntToStr(posPrinter.ColunasFonteExpandida) + " Colunas");
            //posPrinter.ImprimirLinha(LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", posPrinter.ColunasFonteExpandida));
            //posPrinter.ImprimirLinha("</e><c>CONDENSADO: " + IntToStr(posPrinter.ColunasFonteCondensada) + " Colunas");
            //posPrinter.ImprimirLinha(LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", posPrinter.ColunasFonteCondensada));
            posPrinter.ImprimirLinha("</c><n>FONTE NEGRITO</N>");
            posPrinter.ImprimirLinha("<in>FONTE INVERTIDA</in>");
            posPrinter.ImprimirLinha("<S>FONTE SUBLINHADA</s>");
            posPrinter.ImprimirLinha("<i>FONTE ITALICO</i>");
            posPrinter.ImprimirLinha("FONTE NORMAL");
            posPrinter.ImprimirLinha("</linha_simples>");
            posPrinter.ImprimirLinha("<n>LIGA NEGRITO");
            posPrinter.ImprimirLinha("<i>LIGA ITALICO");
            posPrinter.ImprimirLinha("<S>LIGA SUBLINHADA");
            posPrinter.ImprimirLinha("<c>LIGA CONDENSADA");
            posPrinter.ImprimirLinha("<e>LIGA EXPANDIDA");
            posPrinter.ImprimirLinha("</fn>FONTE NORMAL");
            posPrinter.ImprimirLinha("</linha_simples>");
            posPrinter.ImprimirLinha("<e><n>NEGRITO E EXPANDIDA</n></e>");
            posPrinter.ImprimirLinha("</fn>FONTE NORMAL");
            posPrinter.ImprimirLinha("<in><c>INVERTIDA E CONDENSADA</c></in>");
            posPrinter.ImprimirLinha("</fn>FONTE NORMAL");
            posPrinter.ImprimirLinha("</linha_simples>");
            posPrinter.ImprimirLinha("</FB>FONTE TIPO B");
            posPrinter.ImprimirLinha("<n>FONTE NEGRITO</N>");
            posPrinter.ImprimirLinha("<e>FONTE EXPANDIDA</e>");
            posPrinter.ImprimirLinha("<in>FONTE INVERTIDA</in>");
            posPrinter.ImprimirLinha("<S>FONTE SUBLINHADA</s>");
            posPrinter.ImprimirLinha("<i>FONTE ITALICO</i>");
            posPrinter.ImprimirLinha("</FA>FONTE TIPO A");
            posPrinter.ImprimirLinha("</FN>FONTE NORMAL");
            posPrinter.ImprimirLinha("</corte_total>");
        }

        private void btnLeituraStatus_Click(object sender, EventArgs e)
        {
            if (!btnSerial.Enabled)
                ToogleActivate();

            var status = posPrinter.LerStatusImpressora();
            foreach (ACBrPosTipoStatus value in Enum.GetValues(typeof(ACBrPosTipoStatus)))
            {
                if (status.HasFlag(value))
                    txtImpressao.AppendLine(value.ToString());
            }
        }

        #endregion EventHandlers

        private void LoadConfig()
        {
            posPrinter.ConfigLer();

            cbbModelo.SetSelectedValue(posPrinter.ConfigLerValor<ACBrPosPrinterModelo>(ACBrSessao.PosPrinter, "Modelo"));
            cbbPortas.SelectedItem = posPrinter.ConfigLerValor<string>(ACBrSessao.PosPrinter, "Porta");
            nudColunas.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "ColunasFonteNormal");
            nudEspacos.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "EspacoEntreLinhas");
            nudBuffer.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasBuffer");
            nudLinhasPular.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasEntreCupons");
            cbxControlePorta.Checked = posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "ControlePorta");
            cbxCortarPapel.Checked = posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "CortaPapel");
            cbxTraduzirTags.Checked = posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "TraduzirTags");
            cbxIgnorarTags.Checked = posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "IgnorarTags");
            txtArqLog.Text = posPrinter.ConfigLerValor<string>(ACBrSessao.PosPrinter, "ArqLog");
            cbbPaginaCodigo.SetSelectedValue(posPrinter.ConfigLerValor<PosPaginaCodigo>(ACBrSessao.PosPrinter, "PaginaDeCodigo"));

            nudCodbarLargura.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Barras, "LarguraLinha");
            nudCodbarAltura.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Barras, "Altura");
            cbxCodbarExibeNumeros.Checked = posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Barras, "MostrarCodigo");

            nudQRTipo.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_QRCode, "Tipo");
            nudQRLargura.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_QRCode, "LarguraModulo");
            nudQRErrorLevel.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_QRCode, "ErrorLevel");

            nudLogoKC1.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "KeyCode1");
            nudLogoKC2.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "KeyCode2");
            nudLogoFatorX.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "FatorX");
            nudLogoFatorY.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "FatorY");

            nudGVON.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Gaveta, "TempoON");
            nudGVOFF.Value = posPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Gaveta, "TempoOFF");
            cbxGVInvertido.Checked = posPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Gaveta, "SinalInvertido");
        }

        private void SaveConfig()
        {
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "Modelo", cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>());
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "Porta", cbbPortas.Text);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "ColunasFonteNormal", (int)nudColunas.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "EspacoEntreLinhas", (int)nudEspacos.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasBuffer", (int)nudBuffer.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasEntreCupons", (int)nudLinhasPular.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "ControlePorta", cbxControlePorta.Checked);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "CortaPapel", cbxCortarPapel.Checked);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "TraduzirTags", cbxTraduzirTags.Checked);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "IgnorarTags", cbxIgnorarTags.Checked);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "ArqLog", txtArqLog.Text);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "PaginaDeCodigo", cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>());

            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Barras, "LarguraLinha", (int)nudCodbarLargura.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Barras, "Altura", (int)nudCodbarAltura.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Barras, "MostrarCodigo", cbxCodbarExibeNumeros.Checked);

            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_QRCode, "Tipo", (int)nudQRTipo.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_QRCode, "LarguraModulo", (int)nudQRLargura.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_QRCode, "ErrorLevel", (int)nudQRErrorLevel.Value);

            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "KeyCode1", (int)nudLogoKC1.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "KeyCode2", (int)nudLogoKC2.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "FatorX", (int)nudLogoFatorX.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "FatorY", (int)nudLogoFatorY.Value);

            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Gaveta, "TempoON", (int)nudGVON.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Gaveta, "TempoOFF", (int)nudGVOFF.Value);
            posPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Gaveta, "SinalInvertido", cbxGVInvertido.Checked);

            posPrinter.ConfigGravar();
        }

        private void ToogleActivate()
        {
            if (btnSerial.Enabled)
            {
                SaveConfig();
                posPrinter.Ativar();
                btnSerial.Enabled = false;
                btnAtivar.Text = @"Desativar";
            }
            else
            {
                posPrinter.Desativar();
                btnSerial.Enabled = true;
                btnAtivar.Text = @"Ativar";
            }
        }

        #endregion Methods
    }
}