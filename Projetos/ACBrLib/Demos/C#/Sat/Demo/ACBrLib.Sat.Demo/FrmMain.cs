using System;
using System.Drawing.Printing;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PosPrinter;
using ACBrLib.Core.Sat;
using ACBrLib.Core.Extensions;

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
            //acbrSat = new ACBrSat("[Memory]");
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);
            cmbCrypt.EnumDataSource(SSLCryptLib.cryOpenSSL);

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
            cmbImpressao.EnumDataSource(TipoExtrato.tpFortes);
            cbbModelo.EnumDataSource(ACBrPosPrinterModelo.ppTexto);
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

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.OpenFile("Arquivo XSD (*.xsd)|*.xsd|Todo os Arquivos (*.*)|*.*");
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

        private void BtnAtivarSAT_Click(object sender, EventArgs e)
        {
            var ret = acbrSat.AtivarSAT(txtCNPJContribuinte.Text, Convert.ToInt32(txtCodEstFederacao.Text));
            rtbRespostas.AppendText(ret.Resposta);
        }

        private void btnConsultarSAT_Click(object sender, EventArgs e)
        {
            var ret = acbrSat.ConsultarSAT();
            rtbRespostas.AppendLine(ret.Resposta);
        }

        private void btnConsultarStatus_Click(object sender, EventArgs e)
        {
            var ret = acbrSat.ConsultarStatusOperacional();
            rtbRespostas.AppendLine(ret.Resposta);
        }

        private void btnConsultarUltimaSessaoFiscal_Click(object sender, EventArgs e)
        {
            var ret = acbrSat.ConsultarUltimaSessaoFiscal();
            rtbRespostas.AppendLine(ret.Resposta);
        }

        private void btnCriarCFe_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Arquivo Ini CFe (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = acbrSat.CriarCFe(iniPath);
            rtbRespostas.AppendLine(ret.Resposta);
        }

        private void btnCriarEnviarCFe_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Arquivo Ini CFe (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = acbrSat.CriarEnviarCFe(iniPath);
            rtbRespostas.AppendLine(ret.Resposta);
        }

        private void btnEnviarCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var ret = acbrSat.EnviarCFe(xmlPath);
            rtbRespostas.AppendLine(ret.Resposta);
        }

        private void btnCancelarCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var ret = acbrSat.CancelarCFe(xmlPath);
            rtbRespostas.AppendLine(ret.Resposta);
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

        private void btnGerarPDF_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            acbrSat.GerarPDFExtratoVenda(xmlPath, "PDF CFe-SAT");
            rtbRespostas.AppendLine("PDF salvo com sucesso.");
        }

        private void btnGerarPDFCancelamento_Click(object sender, EventArgs e)
        {
            var xmlVenda = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlVenda)) return;

            var xmlCancelamento = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlCancelamento)) return;

            acbrSat.GerarPDFCancelamento(xmlVenda, xmlCancelamento, "PDF CFe-SAT Cancelamento");
            rtbRespostas.AppendLine("PDF Cancelamento salvo com sucesso.");
        }

        private void btnImprimirCFeCanc_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe Venda (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var xmlCancPath = Helpers.OpenFile("Arquivo Xml CFe Cancelamento (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlCancPath)) return;

            acbrSat.ImprimirExtratoCancelamento(xmlPath, xmlCancPath);
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
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            var destinatario = "";
            var eAssunto = "";
            var eNomeArquivo = "";
            var eMenssagem = "";
            if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
            if (InputBox.Show("Envio email", "Digite o Assunto", ref eAssunto) != DialogResult.OK) return;
            if (InputBox.Show("Envio email", "Digite o nome do arquivo PDF", ref eNomeArquivo) != DialogResult.OK) return;
            if (InputBox.Show("Envio email", "Digite digite a mensagem", ref eMenssagem) != DialogResult.OK) return;

            if (string.IsNullOrEmpty(destinatario)) return;

            acbrSat.EnviarEmail(xmlPath, destinatario, eAssunto, eNomeArquivo, eMenssagem, "", "");
        }

        private void btnValidarCFe_Click(object sender, EventArgs e)
        {
            var xmlPath = Helpers.OpenFile("Arquivo Xml CFe (*.xml)|*.xml|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(xmlPath)) return;

            acbrSat.validarCFe(xmlPath);
        }

        private async void btnSalvarPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var nomeArquivo = Helpers.SaveFile("Salvar em PDF (*.pdf)|*.pdf|Todos os Arquivos (*.*)|*.*");

                using (FileStream aStream = File.Create(nomeArquivo))
                {
                    acbrSat.SalvarPDF(aStream);
                    byte[] buffer = new Byte[aStream.Length];
                    await aStream.ReadAsync(buffer, 0, buffer.Length);
                    await aStream.FlushAsync();
                    aStream.Seek(0, SeekOrigin.End);
                    await aStream.WriteAsync(buffer, 0, buffer.Length);
                }
                rtbRespostas.AppendLine($"PDF Salvo em: {nomeArquivo}");
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnClasseAltoNivel_Click(object sender, EventArgs e)
        {
            var CFeSAT = AlimentarDados();

            var ret = acbrSat.CriarEnviarCFe(CFeSAT);
            rtbRespostas.AppendLine(ret.Resposta);

            if (ret.CodigoDeRetorno == 6000)
            {
                acbrSat.ImprimirExtratoVenda(ret.Arquivo);
                rtbRespostas.AppendLine("Impressão efetuada com sucesso.");
            } 
        }

        private void LoadConfig()
        {
            acbrSat.ConfigLer();

            txtDllPath.Text = acbrSat.Config.NomeDLL;
            cmbModeloSat.SetSelectedValue(acbrSat.Config.Modelo);
            txtAtivacao.Text = acbrSat.Config.CodigoDeAtivacao;
            nunVersaoCFe.Value = acbrSat.Config.SatConfig.infCFe_versaoDadosEnt;
            nunPaginaCodigo.Value = acbrSat.Config.SatConfig.PaginaDeCodigo;
            txtSchemaPath.Text = acbrSat.Config.SatConfig.ArqSchema;
            cmbXmlSign.SetSelectedValue(acbrSat.Config.SatConfig.SSLXmlSignLib);
            cmbCrypt.SetSelectedValue(acbrSat.Config.DFe.SSLCryptLib);
            txtSignAc.Text = acbrSat.Config.SignAC;
            chkSaveCFe.Checked = acbrSat.Config.Arquivos.SalvarCFe;
            chkSaveEnvio.Checked = acbrSat.Config.Arquivos.SalvarEnvio;
            chkSaveCFeCanc.Checked = acbrSat.Config.Arquivos.SalvarCFeCanc;
            chkSepararCNPJ.Checked = acbrSat.Config.Arquivos.SepararPorCNPJ;
            chkSepararData.Checked = acbrSat.Config.Arquivos.SepararPorDia;

            //Extrato
            cmbImpressao.SetSelectedValue(acbrSat.Config.Extrato.Tipo);
            nudCopias.Value = acbrSat.Config.Extrato.Copias;
            txtSoftwareHouse.Text = acbrSat.Config.Sistema.Nome;
            cbbImpressora.Text = acbrSat.Config.Extrato.Impressora;
            txtSite.Text = acbrSat.Config.Emissor.WebSite;
            chkPreview.Checked = acbrSat.Config.Extrato.MostraPreview;
            chkSetup.Checked = acbrSat.Config.Extrato.MostraSetup;
            chkUsaCodigoEanImpressao.Checked = acbrSat.Config.Extrato.ImprimeCodigoEan;
            chkImprimeEmUmaLinha.Checked = acbrSat.Config.Extrato.ImprimeEmUmaLinha;
            chkLogoLateral.Checked = acbrSat.Config.Extrato.ImprimeLogoLateral;
            chkQrCodeLateral.Checked = acbrSat.Config.Extrato.ImprimeQRCodeLateral;

            //PosPrinter
            cbbModelo.SetSelectedValue(acbrSat.Config.PosPrinter.Modelo);
            cbbPortas.Text = acbrSat.Config.PosPrinter.Porta;
            cbbPaginaCodigo.SetSelectedValue(acbrSat.Config.PosPrinter.PaginaDeCodigo);
            nudColunas.Value = acbrSat.Config.PosPrinter.ColunasFonteNormal;
            nudEspacos.Value = acbrSat.Config.PosPrinter.EspacoEntreLinhas;
            nudBuffer.Value = acbrSat.Config.PosPrinter.LinhasBuffer;
            nudLinhasPular.Value = acbrSat.Config.PosPrinter.LinhasEntreCupons;
            cbxControlePorta.Checked = acbrSat.Config.PosPrinter.ControlePorta;
            cbxCortarPapel.Checked = acbrSat.Config.PosPrinter.CortaPapel;
            cbxTraduzirTags.Checked = acbrSat.Config.PosPrinter.TraduzirTags;
            cbxIgnorarTags.Checked = acbrSat.Config.PosPrinter.IgnorarTags;

            //Mail
            txtNome.Text = acbrSat.Config.Email.Nome;
            txtEmail.Text = acbrSat.Config.Email.Conta;
            txtUsuario.Text = acbrSat.Config.Email.Usuario;
            txtSenha.Text = acbrSat.Config.Email.Senha;
            txtHost.Text = acbrSat.Config.Email.Servidor;
            nudPorta.Text = acbrSat.Config.Email.Porta;
            ckbSSL.Checked = acbrSat.Config.Email.SSL;
            ckbTLS.Checked = acbrSat.Config.Email.TLS;
        }

        private void SaveConfig()
        {
            acbrSat.Config.NomeDLL = txtDllPath.Text;
            acbrSat.Config.Modelo = cmbModeloSat.GetSelectedValue<SATModelo>();
            acbrSat.Config.CodigoDeAtivacao = txtAtivacao.Text;
            acbrSat.Config.SatConfig.infCFe_versaoDadosEnt = nunVersaoCFe.Value;
            acbrSat.Config.SatConfig.PaginaDeCodigo = (ushort)nunPaginaCodigo.Value;
            acbrSat.Config.SatConfig.ArqSchema = txtSchemaPath.Text;
            acbrSat.Config.SatConfig.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
            acbrSat.Config.SignAC = txtSignAc.Text;
            acbrSat.Config.Arquivos.SalvarCFe = chkSaveCFe.Checked;
            acbrSat.Config.Arquivos.SalvarEnvio = chkSaveEnvio.Checked;
            acbrSat.Config.Arquivos.SalvarCFeCanc = chkSaveCFeCanc.Checked;
            acbrSat.Config.Arquivos.SepararPorCNPJ = chkSepararCNPJ.Checked;
            acbrSat.Config.Arquivos.SepararPorDia = chkSepararData.Checked;

            //Impressão
            acbrSat.Config.Extrato.Tipo = cmbImpressao.GetSelectedValue<TipoExtrato>();
            acbrSat.Config.Extrato.Copias = (int)nudCopias.Value;
            acbrSat.Config.Sistema.Nome = txtSoftwareHouse.Text;
            acbrSat.Config.Extrato.Impressora = cbbImpressora.Text;
            acbrSat.Config.Emissor.WebSite = txtSite.Text;
            acbrSat.Config.Extrato.MostraPreview = chkPreview.Checked;
            acbrSat.Config.Extrato.MostraSetup = chkSetup.Checked;
            acbrSat.Config.Extrato.ImprimeCodigoEan = chkUsaCodigoEanImpressao.Checked;
            acbrSat.Config.Extrato.ImprimeEmUmaLinha = chkImprimeEmUmaLinha.Checked;
            acbrSat.Config.Extrato.ImprimeLogoLateral = chkLogoLateral.Checked;
            acbrSat.Config.Extrato.ImprimeQRCodeLateral = chkQrCodeLateral.Checked;

            //PosPrinter
            acbrSat.Config.PosPrinter.Modelo = cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>();
            acbrSat.Config.PosPrinter.Porta = cbbPortas.Text;
            acbrSat.Config.PosPrinter.ColunasFonteNormal = (int)nudColunas.Value;
            acbrSat.Config.PosPrinter.EspacoEntreLinhas = (int)nudEspacos.Value;
            acbrSat.Config.PosPrinter.LinhasBuffer = (int)nudBuffer.Value;
            acbrSat.Config.PosPrinter.LinhasEntreCupons = (int)nudLinhasPular.Value;
            acbrSat.Config.PosPrinter.ControlePorta = cbxControlePorta.Checked;
            acbrSat.Config.PosPrinter.CortaPapel = cbxCortarPapel.Checked;
            acbrSat.Config.PosPrinter.TraduzirTags = cbxTraduzirTags.Checked;
            acbrSat.Config.PosPrinter.IgnorarTags = cbxIgnorarTags.Checked;
            acbrSat.Config.PosPrinter.PaginaDeCodigo = cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>();

            //Mail
            acbrSat.Config.Email.Nome = txtNome.Text;
            acbrSat.Config.Email.Conta = txtEmail.Text;
            acbrSat.Config.Email.Usuario = txtUsuario.Text;
            acbrSat.Config.Email.Senha = txtSenha.Text;
            acbrSat.Config.Email.Servidor = txtHost.Text;
            acbrSat.Config.Email.Porta = nudPorta.Text;
            acbrSat.Config.Email.SSL = ckbSSL.Checked;
            acbrSat.Config.Email.TLS = ckbTLS.Checked;

            acbrSat.ConfigGravar();
        }

        private string AlimentarDados()
        {
            var CFe = new CupomFiscal();

            //infCFe
            CFe.InfCFe.versao = "0.08";

            //Identificação
            CFe.Identificacao.CNPJ = "16716114000172";
            CFe.Identificacao.signAC = "SGR-SAT SISTEMA DE GESTAO E RETAGUARDA DO SAT";
            CFe.Identificacao.numeroCaixa = "01";

            //Emitente
            CFe.Emitente.CNPJ = "14200166000166";
            CFe.Emitente.IE = "111111111111";
            CFe.Emitente.IM = "111111";

            //Destinatário
            CFe.Destinatario.CNPJCPF = "18760540000139";
            CFe.Destinatario.xNome = "Projeto ACBr";

            //Entrega
            CFe.Entrega.xLgr = "Rua Cel. Aureliano de Camargo";
            CFe.Entrega.nro = "973";
            CFe.Entrega.xBairro = "Centro";
            CFe.Entrega.xMun = "Tatui";
            CFe.Entrega.UF = "SP";

            //Produto
            var produto = new ProdutoSat
            {
                cProd = "1189",
                infAdProd = "Teste de Produto",
                xProd = "OVO VERMELHO",
                NCM = "04072100",
                CFOP = "5102",
                uCom = "DZ",
                Combustivel = false,
                qCom = Convert.ToInt32(510),
                vUnCom = Convert.ToInt32(1.00), 
                indRegra = IndRegra.irArredondamento,
                vDesc = 0,
                vOutro = 0,
                vItem12741 = Convert.ToInt32(137.00),
            };

            produto.ICMS.orig = OrigemMercadoria.oeNacional;
            produto.ICMS.CSOSN = CSOSNIcms.csosn500;
            produto.PIS.CST = CSTPIS.pis01;
            produto.COFINS.CST = CSTCofins.cof01;

            CFe.Total.vCFeLei12741 = Convert.ToInt32(1.00);

            CFe.Total.vDescSubtot = Convert.ToInt32(1.00);

            CFe.Produtos.Add(produto);

            var pagamento = new PagamentoSat
            {
                cMP = CodigoMP.mpDinheiro,
                vMP = 1400,
            };

            CFe.Pagamentos.Add(pagamento);

            CFe.DadosAdicionais.infCpl = "Exemplo Preenchimento Classe Alto Nível";

            return CFe.ToString();
        }

        #endregion Methods
    }
}