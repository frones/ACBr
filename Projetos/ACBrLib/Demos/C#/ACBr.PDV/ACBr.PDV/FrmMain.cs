using System;
using System.Drawing.Printing;
using System.IO;
using System.IO.Compression;
using System.IO.Ports;
using System.Linq;
using System.Windows.Forms;
using ACBr.PDV.Model;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.BAL;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;
using ACBrLib.Core.PosPrinter;
using ACBrLib.Core.Sat;
using ACBrLib.Core.Serial;
using ACBr.Net.Core.Extensions;

namespace ACBr.PDV
{
    public partial class FrmMain : Form
    {
        #region Fields

        private CaixaPDV Caixa;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();
        }

        private void FrmMain_FormClosed(object sender, FormClosedEventArgs e)
        {
            Caixa?.Dispose();
        }

        private void FmMain_Load(object sender, EventArgs e)
        {
            //Hide Tabs Header
            tbcViews.HideTabHeaders();
            tbcViews.SelectedTab = tbpVenda;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            Caixa = new CaixaPDV();
            Caixa.OnAtualizarBobina += (c, args) => AtualizarBobina();
            Caixa.OnAtualizarMensagem += (c, args) => lblMensagem.Text = Caixa.MensagemCaixa;
            Caixa.OnStatusChange += (c, args) => btnConfiguracoes.Enabled = Caixa.Status.IsIn(StatusVenda.Finalizada, StatusVenda.Livre, StatusVenda.Cancelada);
            Caixa.OnRetornoReceived += (c, args) => FrmRetorno.ShowRetorno(this, args.Retorno);
            Caixa.Inicializar();

            Inicializar();
        }

        #endregion Constructors

        #region Methods

        private void Inicializar()
        {
            //Emitente
            cmbEmitCRT.EnumDataSource(CRT.crtSimplesNacional);
            cmbDFeTipo.EnumDataSource(TipoDFe.SAT);

            // NFCe
            cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
            cmbModeloDocumento.EnumDataSource(ModeloNFe.moNFe);
            cmbVersaoDF.EnumDataSource(VersaoNFe.ve400);
            cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
            cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
            cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

            cmbUfDestino.SelectedItem = "SP";
            cmbSSlType.EnumDataSource(SSLType.LT_TLSv1_2);

            cmbTipoDANFCe.EnumDataSource(TipoRelatorioBobina.tpEscPos);
            cmbEventoDANFCe.EnumDataSource(TipoRelatorioEvento.evBobina);

            //SAT
            cmbModeloSat.EnumDataSource(SATModelo.satNenhum);
            cmbImpressao.EnumDataSource(TipoExtrato.tpFortes);
            cmbSATRegTrib.SelectedIndex = 0;
            cmbSATRegTribISSQN.SelectedIndex = 6;
            cmbSATRatIISQN.SelectedIndex = 1;

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cbbImpressora.Items.Add(printer);
                cmbDANFCeImpressora.Items.Add(printer);
            }

            //PosPrinter
            cbbPortas.Items.AddRange(Caixa.PosPrinter.AcharPortas());
            cbbPortas.Items.Add(@"c:\temp\posprinter.txt");

            cbbPortas.SelectedIndex = cbbPortas.Items.Count - 1;

            cbbModelo.EnumDataSource(ACBrPosPrinterModelo.ppTexto);
            cbbPaginaCodigo.EnumDataSource(PosPaginaCodigo.pc850);

            cmbPosBaud.Items.Add(1200);
            cmbPosBaud.Items.Add(2400);
            cmbPosBaud.Items.Add(4800);
            cmbPosBaud.Items.Add(9600);
            cmbPosBaud.Items.Add(19200);
            cmbPosBaud.Items.Add(38400);
            cmbPosBaud.Items.Add(57600);
            cmbPosBaud.Items.Add(115200);

            cmbPosDatabits.Items.Add(5);
            cmbPosDatabits.Items.Add(6);
            cmbPosDatabits.Items.Add(7);
            cmbPosDatabits.Items.Add(8);

            cmbPosStopbits.EnumDataSource(SerialStopBytes.One);
            cmbPosParity.EnumDataSource(SerialParity.None);
            cmbPosHandshaking.EnumDataSource(SerialHandShake.Nenhum);

            //Balança
            cmbModelo.EnumDataSource(ACBrBALModelo.balNenhum);

            cmbPorta.Items.AddRange(SerialPort.GetPortNames());

            cmbBaud.Items.Add(1200);
            cmbBaud.Items.Add(2400);
            cmbBaud.Items.Add(4800);
            cmbBaud.Items.Add(9600);
            cmbBaud.Items.Add(19200);
            cmbBaud.Items.Add(38400);
            cmbBaud.Items.Add(57600);
            cmbBaud.Items.Add(115200);

            cmbDatabits.Items.Add(5);
            cmbDatabits.Items.Add(6);
            cmbDatabits.Items.Add(7);
            cmbDatabits.Items.Add(8);

            cmbStopbits.EnumDataSource(SerialStopBytes.One);
            cmbParity.EnumDataSource(SerialParity.None);
            cmbHandshaking.EnumDataSource(SerialHandShake.Nenhum);

            dgvItensVenda.AutoGenerateColumns = false;
            dgvPagamentos.AutoGenerateColumns = false;

            LoadConfig();
        }

        private void ShowPDV()
        {
            tbcConfiguracoes.SelectedTab = tbpEmitente;
            tbcNFe.SelectedTab = tbpNFeGeral;
            tbcViews.SelectedTab = tbpVenda;

            plnBobina.Enabled = true;
            plnBobina.Show();
        }

        private void AtualizarBobina()
        {
            lstBobina.Items.Clear();

            foreach (var reg in Caixa.Bobina)
            {
                if (!string.IsNullOrEmpty(reg.Linha1))
                    lstBobina.Items.Add(reg.Linha1);
                if (!string.IsNullOrEmpty(reg.Linha2))
                    lstBobina.Items.Add(reg.Linha2);
            }

            lblMensagem.Text = Caixa.MensagemCaixa;

            dgvItensVenda.DataSource = null;
            dgvItensVenda.DataSource = Caixa.Itens;

            dgvPagamentos.DataSource = null;
            dgvPagamentos.DataSource = Caixa.Pagamentos;

            lblTotal.Text = $@"{Caixa.ValorVenda:C2}";
            lblPago.Text = $@"{Caixa.ValorPago:C2}";
            lblTroco.Text = $@"{Caixa.Troco:C2}";
        }

        private void ExportarConfig()
        {
            var file = Helpers.SaveFile("Arquivo de Configuração (*.cfz)|*.cfz");
            if (string.IsNullOrEmpty(file)) return;

            SplashScreenManager.Show<FrmWait>();

            SaveConfig();

            try
            {
                using (var stream = new FileStream(file, FileMode.OpenOrCreate))
                using (var arquivo = new ZipArchive(stream, ZipArchiveMode.Update))
                {
                    if (arquivo.Entries.Any())
                    {
                        ZipArchiveEntry entry;
                        do
                        {
                            entry = arquivo.Entries.FirstOrDefault();
                            entry?.Delete();
                        } while (entry != null);
                    }

                    arquivo.CreateEntryFromFile(Path.Combine(Application.StartupPath, "ACBrLib.ini"), "ACBrLib.ini");
                    arquivo.CreateEntryFromFile(Path.Combine(Application.StartupPath, "acbr.config"), "acbr.config");
                }
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void ImportarConfig()
        {
            var file = Helpers.OpenFile("Arquivo de Configuração (*.cfz)|*.cfz");

            SplashScreenManager.Show<FrmWait>();

            try
            {
                if (string.IsNullOrEmpty(file)) return;

                using (var stream = new FileStream(file, FileMode.Open))
                using (var arquivo = new ZipArchive(stream, ZipArchiveMode.Read))
                {
                    foreach (var entry in arquivo.Entries)
                        entry.ExtractToFile(Path.Combine(Application.StartupPath, entry.FullName), true);
                }

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig()
        {
            #region Emitente

            Configuracao.Instance.Load();

            txtEmitCNPJ.Text = Configuracao.Instance.Emitente.CNPJ;
            txtEmitIE.Text = Configuracao.Instance.Emitente.IE;
            txtEmitIM.Text = Configuracao.Instance.Emitente.IM;
            txtEmitRazao.Text = Configuracao.Instance.Emitente.Razao;
            txtEmitFantasia.Text = Configuracao.Instance.Emitente.Fantasia;
            txtEmitFone.Text = Configuracao.Instance.Emitente.Fone;
            cmbEmitCRT.SetSelectedValue(Configuracao.Instance.Emitente.CRT);
            txtEmitCEP.Text = Configuracao.Instance.Emitente.CEP;
            txtEmitLogradouro.Text = Configuracao.Instance.Emitente.Logradouro;
            txtEmitNumero.Text = Configuracao.Instance.Emitente.Numero;
            txtEmitComplemento.Text = Configuracao.Instance.Emitente.Complemento;
            txtEmitBairro.Text = Configuracao.Instance.Emitente.Bairro;
            txtEmiCidadeCod.Text = Configuracao.Instance.Emitente.CidadeCod;
            txtEmitCidade.Text = Configuracao.Instance.Emitente.Cidade;
            cmbEmitUF.SelectedItem = Configuracao.Instance.Emitente.UF;

            #endregion Emitente

            #region Documento Fiscal

            cmbDFeTipo.SetSelectedValue(Configuracao.Instance.DFe.TipoDocumento);
            nudDFeSerie.Value = Configuracao.Instance.DFe.Serie;
            nudDFeNum.Value = Configuracao.Instance.DFe.NumeroAtual;

            #endregion Documento Fiscal

            #region NFe

            Caixa.NFe.ConfigLer();

            //Config Geral
            ckbAtualizarXML.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "AtualizarXMLCancelado");
            ckbExibirErroSchema.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "ExibirErroSchema");
            txtFormatoAlerta.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "FormatoAlerta");
            cmbFormaEmissao.SetSelectedValue(Caixa.NFe.ConfigLerValor<TipoEmissao>(ACBrSessao.NFe, "FormaEmissao"));
            cmbModeloDocumento.SetSelectedValue(Caixa.NFe.ConfigLerValor<ModeloNFe>(ACBrSessao.NFe, "ModeloDF"));
            cmbVersaoDF.SetSelectedValue(Caixa.NFe.ConfigLerValor<VersaoNFe>(ACBrSessao.NFe, "VersaoDF"));
            ckbRetirarAcentos.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "RetirarAcentos");
            ckbSalvar.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarWS");
            txtLogs.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathSalvar");
            txtSchemaPath.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathSchemas");
            txtIdCSC.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "IdCSC");
            txtCSC.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "CSC");

            //Config Webservice
            cmbUfDestino.SelectedItem = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.DFe, "UF");
            cmbSSlType.SetSelectedValue(Caixa.NFe.ConfigLerValor<SSLType>(ACBrSessao.NFe, "SSLType"));
            nudTimeOut.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.NFe, "Timeout");

            var ambiente = Caixa.NFe.ConfigLerValor<TipoAmbiente>(ACBrSessao.NFe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "Visualizar");
            ckbSalvarSOAP.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarWS");
            ckbAjustarAut.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "AjustaAguardaConsultaRet");
            nudAguardar.Value = Caixa.NFe.ConfigLerValor<int>(ACBrSessao.NFe, "AguardarConsultaRet");
            nudTentativas.Value = Caixa.NFe.ConfigLerValor<int>(ACBrSessao.NFe, "Tentativas");
            nudIntervalos.Value = Caixa.NFe.ConfigLerValor<int>(ACBrSessao.NFe, "IntervaloTentativas");
            txtProxyServidor.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");

            //Config Certificado
            cmbCrypt.SetSelectedValue(Caixa.NFe.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(Caixa.NFe.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(Caixa.NFe.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtCertPassword.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");

            //Config Arquivos
            ckbSalvarArqs.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarGer");
            ckbPastaMensal.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SepararPorMes");
            ckbAdicionaLiteral.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "AdicionarLiteral");
            ckbEmissaoPathNFe.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "EmissaoPathNFe");
            ckbSalvaPathEvento.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarArq");
            ckbSepararPorCNPJ.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SepararPorCNPJ");
            ckbSepararPorModelo.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SepararPorModelo");
            txtArqNFe.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathNFe");
            txtArqInu.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathInu");
            txtArqEvento.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathEvento");

            //Config Documento Auxiliar
            txtLogomarca.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.DANFE, "PathLogo");
            nudDANFCeCopias.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.DANFE, "Copias");
            cmbDANFCeImpressora.Text = Caixa.NFe.ConfigLerValor<string>(ACBrSessao.DANFE, "Impressora");
            chkNFCePreview.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFE, "MostraPreview");
            chkNFCeSetup.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFE, "MostraSetup");
            cmbTipoDANFCe.SetSelectedValue(Caixa.NFe.ConfigLerValor<TipoRelatorioBobina>(ACBrSessao.DANFENFCe, "TipoRelatorioBobina"));
            cmbEventoDANFCe.SetSelectedValue(Caixa.NFe.ConfigLerValor<TipoRelatorioEvento>(ACBrSessao.DANFENFCe, "TipoRelatorioEvento"));
            nudDANFCeBobina.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.DANFENFCe, "LarguraBobina");
            chkNFCeImprimeDescAcr.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFENFCe, "ImprimeDescAcrescItem");
            chkNFCeImprimeItens.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFENFCe, "ImprimeItens");
            chkNFCeQrCodeLateral.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFENFCe, "ImprimeQRCodeLateral");
            chkNFCeLogoLateral.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFENFCe, "ImprimeLogoLateral");
            chkNFCeImprimeUmaLinha.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFENFCe, "ImprimeEmUmaLinha");
            chkNFCeImprimeDuasLinhas.Checked = Caixa.NFe.ConfigLerValor<bool>(ACBrSessao.DANFENFCe, "ImprimeEmDuasLinhas");
            nudNFCeMSuperior.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.DANFENFCe, "MargemSuperior");
            nudNFCeMInferior.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.DANFENFCe, "MargemInferior");
            nudNFCeMEsquerda.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.DANFENFCe, "MargemEsquerda");
            nudNFCeMDireita.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.DANFENFCe, "MargemDireita");

            #endregion NFe

            #region SAT

            Caixa.SAT.ConfigLer();

            txtDllPath.Text = Caixa.SAT.ConfigLerValor<string>(ACBrSessao.SAT, "NomeDLL");
            cmbModeloSat.SetSelectedValue(Caixa.SAT.ConfigLerValor<SATModelo>(ACBrSessao.SAT, "Modelo"));
            txtAtivacao.Text = Caixa.SAT.ConfigLerValor<string>(ACBrSessao.SAT, "CodigoDeAtivacao");
            nunVersaoCFe.Value = Caixa.SAT.ConfigLerValor<decimal>(ACBrSessao.SATConfig, "infCFe_versaoDadosEnt");
            nunPaginaCodigo.Value = Caixa.SAT.ConfigLerValor<decimal>(ACBrSessao.SATConfig, "PaginaDeCodigo");
            txtSoftCNPJ.Text = Caixa.SAT.ConfigLerValor<string>(ACBrSessao.SATConfig, "ide_CNPJ");
            cmbSATRegTrib.SelectedIndex = Caixa.SAT.ConfigLerValor<int>(ACBrSessao.SATConfig, "emit_cRegTrib");
            cmbSATRegTribISSQN.SelectedIndex = Caixa.SAT.ConfigLerValor<int>(ACBrSessao.SATConfig, "emit_cRegTribISSQN");
            cmbSATRatIISQN.SelectedIndex = Caixa.SAT.ConfigLerValor<int>(ACBrSessao.SATConfig, "emit_indRatISSQN");
            txtSignAc.Text = Caixa.SAT.ConfigLerValor<string>(ACBrSessao.SAT, "SignAC");
            chkSaveCFe.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SalvarCFe");
            chkSaveEnvio.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SalvarEnvio");
            chkSaveCFeCanc.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SalvarCFeCanc");
            chkSepararCNPJ.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SepararPorCNPJ");
            chkSepararData.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.SATConfigArquivos, "SepararPorDia");

            //Extrato
            cmbImpressao.SetSelectedValue(Caixa.SAT.ConfigLerValor<TipoExtrato>(ACBrSessao.Extrato, "Tipo"));
            nudCopias.Value = Caixa.SAT.ConfigLerValor<int>(ACBrSessao.Extrato, "Copias");
            cbbImpressora.Text = Caixa.SAT.ConfigLerValor<string>(ACBrSessao.Extrato, "Impressora");
            txtSatLogo.Text = Caixa.SAT.ConfigLerValor<string>(ACBrSessao.Extrato, "PathLogo");
            nudSatMSuperior.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.Extrato, "MargemSuperior");
            nudSatMInferior.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.Extrato, "MargemInferior");
            nudSatMEsquerda.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.Extrato, "MargemEsquerda");
            nudSatMDireita.Value = Caixa.NFe.ConfigLerValor<decimal>(ACBrSessao.Extrato, "MargemDireita");
            chkPreview.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "MostraPreview");
            chkSetup.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "MostraSetup");
            chkUsaCodigoEanImpressao.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "ImprimeCodigoEan");
            chkImprimeEmUmaLinha.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "ImprimeEmUmaLinha");
            chkLogoLateral.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "ImprimeLogoLateral");
            chkQrCodeLateral.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "ImprimeQRCodeLateral");
            chkExpandirLogo.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "ExpandeLogoMarca");
            chkExibirLogo.Checked = Caixa.SAT.ConfigLerValor<bool>(ACBrSessao.Extrato, "LogoVisible");

            #endregion SAT

            #region PosPrinter

            Caixa.PosPrinter.ConfigLer();

            //
            txtSoftwareHouse.Text = Caixa.PosPrinter.ConfigLerValor<string>(ACBrSessao.Sistema, "Nome");
            txtWebsite.Text = Caixa.PosPrinter.ConfigLerValor<string>(ACBrSessao.Emissor, "WebSite");

            cbbModelo.SetSelectedValue(Caixa.PosPrinter.ConfigLerValor<ACBrPosPrinterModelo>(ACBrSessao.PosPrinter, "Modelo"));
            cbbPortas.SelectedItem = Caixa.PosPrinter.ConfigLerValor<string>(ACBrSessao.PosPrinter, "Porta");
            nudColunas.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "ColunasFonteNormal");
            nudEspacos.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "EspacoEntreLinhas");
            nudBuffer.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasBuffer");
            nudLinhasPular.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasEntreCupons");
            cbxControlePorta.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "ControlePorta");
            cbxCortarPapel.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "CortaPapel");
            cbxTraduzirTags.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "TraduzirTags");
            cbxIgnorarTags.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "IgnorarTags");
            txtArqLog.Text = Caixa.PosPrinter.ConfigLerValor<string>(ACBrSessao.PosPrinter, "ArqLog");
            cbbPaginaCodigo.SetSelectedValue(Caixa.PosPrinter.ConfigLerValor<PosPaginaCodigo>(ACBrSessao.PosPrinter, "PaginaDeCodigo"));

            nudCodbarLargura.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Barras, "LarguraLinha");
            nudCodbarAltura.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Barras, "Altura");
            cbxCodbarExibeNumeros.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Barras, "MostrarCodigo");

            nudQRTipo.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_QRCode, "Tipo");
            nudQRLargura.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_QRCode, "LarguraModulo");
            nudQRErrorLevel.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_QRCode, "ErrorLevel");

            nudLogoKC1.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "KeyCode1");
            nudLogoKC2.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "KeyCode2");
            nudLogoFatorX.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "FatorX");
            nudLogoFatorY.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Logo, "FatorY");

            nudGVON.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Gaveta, "TempoON");
            nudGVOFF.Value = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Gaveta, "TempoOFF");
            cbxGVInvertido.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Gaveta, "SinalInvertido");

            cmbPosBaud.SelectedItem = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Device, "Baud");
            cmbPosDatabits.SelectedItem = Caixa.PosPrinter.ConfigLerValor<int>(ACBrSessao.PosPrinter_Device, "Data");
            cmbPosParity.SelectedItem = Caixa.PosPrinter.ConfigLerValor<SerialParity>(ACBrSessao.PosPrinter_Device, "Parity");
            cmbPosStopbits.SelectedItem = Caixa.PosPrinter.ConfigLerValor<SerialStopBytes>(ACBrSessao.PosPrinter_Device, "Stop");
            cmbPosHandshaking.SelectedItem = Caixa.PosPrinter.ConfigLerValor<SerialHandShake>(ACBrSessao.PosPrinter_Device, "HandShake");
            nudPosMaxBand.Value = Caixa.PosPrinter.ConfigLerValor<decimal>(ACBrSessao.PosPrinter_Device, "MaxBandwidth");
            nudPosBytesCount.Value = Caixa.PosPrinter.ConfigLerValor<decimal>(ACBrSessao.PosPrinter_Device, "SendBytesCount");
            nudPosIntervalo.Value = Caixa.PosPrinter.ConfigLerValor<decimal>(ACBrSessao.PosPrinter_Device, "SendBytesInterval");
            chkPosSoftFlow.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Device, "SoftFlow");
            chkPosHardFlow.Checked = Caixa.PosPrinter.ConfigLerValor<bool>(ACBrSessao.PosPrinter_Device, "HardFlow");

            #endregion PosPrinter

            #region Bal

            Caixa.Bal.ConfigLer();

            cmbModelo.SetSelectedValue(Caixa.Bal.ConfigLerValor<ACBrBALModelo>(ACBrSessao.BAL, "Modelo"));
            cmbPorta.Text = Caixa.Bal.ConfigLerValor<string>(ACBrSessao.BAL, "Porta");
            cmbBaud.SelectedItem = Caixa.Bal.ConfigLerValor<int>(ACBrSessao.BAL_Device, "Baud");
            cmbDatabits.SelectedItem = Caixa.Bal.ConfigLerValor<int>(ACBrSessao.BAL_Device, "Data");
            cmbParity.SelectedItem = Caixa.Bal.ConfigLerValor<SerialParity>(ACBrSessao.BAL_Device, "Parity");
            cmbStopbits.SelectedItem = Caixa.Bal.ConfigLerValor<SerialStopBytes>(ACBrSessao.BAL_Device, "Stop");
            cmbHandshaking.SelectedItem = Caixa.Bal.ConfigLerValor<SerialHandShake>(ACBrSessao.BAL_Device, "HandShake");
            nudMaxBand.Value = Caixa.Bal.ConfigLerValor<decimal>(ACBrSessao.BAL_Device, "MaxBandwidth");
            nudBytesCount.Value = Caixa.Bal.ConfigLerValor<decimal>(ACBrSessao.BAL_Device, "SendBytesCount");
            nudIntervalo.Value = Caixa.Bal.ConfigLerValor<decimal>(ACBrSessao.BAL_Device, "SendBytesInterval");
            chkSoftFlow.Checked = Caixa.Bal.ConfigLerValor<bool>(ACBrSessao.BAL_Device, "SoftFlow");
            chkHardFlow.Checked = Caixa.Bal.ConfigLerValor<bool>(ACBrSessao.BAL_Device, "HardFlow");

            #endregion Bal
        }

        private void SaveConfig()
        {
            #region Emitente

            Configuracao.Instance.Emitente.CNPJ = txtEmitCNPJ.Text.OnlyNumbers(); ;
            Configuracao.Instance.Emitente.IE = txtEmitIE.Text.OnlyNumbers(); ;
            Configuracao.Instance.Emitente.IM = txtEmitIM.Text.OnlyNumbers(); ;
            Configuracao.Instance.Emitente.Razao = txtEmitRazao.Text;
            Configuracao.Instance.Emitente.Fantasia = txtEmitFantasia.Text;
            Configuracao.Instance.Emitente.Fone = txtEmitFone.Text;
            Configuracao.Instance.Emitente.CRT = cmbEmitCRT.GetSelectedValue<CRT>();
            Configuracao.Instance.Emitente.CEP = txtEmitCEP.Text;
            Configuracao.Instance.Emitente.Logradouro = txtEmitLogradouro.Text;
            Configuracao.Instance.Emitente.Numero = txtEmitNumero.Text;
            Configuracao.Instance.Emitente.Complemento = txtEmitComplemento.Text;
            Configuracao.Instance.Emitente.Bairro = txtEmitBairro.Text;
            Configuracao.Instance.Emitente.CidadeCod = txtEmiCidadeCod.Text;
            Configuracao.Instance.Emitente.Cidade = txtEmitCidade.Text;
            Configuracao.Instance.Emitente.UF = cmbEmitUF.Text;

            #endregion Emitente

            #region Documento Fiscal

            Configuracao.Instance.DFe.TipoDocumento = cmbDFeTipo.GetSelectedValue<TipoDFe>();
            Configuracao.Instance.DFe.Serie = (int)nudDFeSerie.Value;
            Configuracao.Instance.DFe.NumeroAtual = (int)nudDFeNum.Value;

            Configuracao.Instance.Save();

            #endregion Documento Fiscal

            #region PosPrinter

            // Configurações de Sistema, é adicionado aqui pois vai seer lido pelas outras libs depois
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.Sistema, "Nome", txtSoftwareHouse.Text);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.Emissor, "WebSite", txtWebsite.Text);

            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "Modelo", cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>());
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "Porta", cbbPortas.Text);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "ColunasFonteNormal", (int)nudColunas.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "EspacoEntreLinhas", (int)nudEspacos.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasBuffer", (int)nudBuffer.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasEntreCupons", (int)nudLinhasPular.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "ControlePorta", cbxControlePorta.Checked);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "CortaPapel", cbxCortarPapel.Checked);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "TraduzirTags", cbxTraduzirTags.Checked);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "IgnorarTags", cbxIgnorarTags.Checked);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "ArqLog", txtArqLog.Text);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter, "PaginaDeCodigo", cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>());

            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Barras, "LarguraLinha", (int)nudCodbarLargura.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Barras, "Altura", (int)nudCodbarAltura.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Barras, "MostrarCodigo", cbxCodbarExibeNumeros.Checked);

            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_QRCode, "Tipo", (int)nudQRTipo.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_QRCode, "LarguraModulo", (int)nudQRLargura.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_QRCode, "ErrorLevel", (int)nudQRErrorLevel.Value);

            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "KeyCode1", (int)nudLogoKC1.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "KeyCode2", (int)nudLogoKC2.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "FatorX", (int)nudLogoFatorX.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Logo, "FatorY", (int)nudLogoFatorY.Value);

            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Gaveta, "TempoON", (int)nudGVON.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Gaveta, "TempoOFF", (int)nudGVOFF.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Gaveta, "SinalInvertido", cbxGVInvertido.Checked);

            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "Baud", cmbPosBaud.SelectedItem);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "Data", cmbPosDatabits.SelectedItem);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "Parity", cmbPosParity.SelectedItem);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "Stop", cmbPosStopbits.SelectedItem);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "HandShake", cmbPosHandshaking.SelectedItem);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "MaxBandwidth", nudPosMaxBand.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "SendBytesCount", nudPosBytesCount.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "SendBytesInterval", nudPosIntervalo.Value);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "SoftFlow", chkPosSoftFlow.Checked);
            Caixa.PosPrinter.ConfigGravarValor(ACBrSessao.PosPrinter_Device, "HardFlow", chkPosHardFlow.Checked);

            Caixa.PosPrinter.ConfigGravar();

            #endregion PosPrinter

            #region NFe

            //Carregando a config do PosPrinter
            Caixa.NFe.ConfigLer();

            //Config Geral
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "AtualizarXMLCancelado", ckbAtualizarXML.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "ExibirErroSchema", ckbExibirErroSchema.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "FormatoAlerta", txtFormatoAlerta.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "FormaEmissao", cmbFormaEmissao.GetSelectedValue<TipoEmissao>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "ModeloDF", cmbModeloDocumento.GetSelectedValue<ModeloNFe>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "VersaoDF", cmbVersaoDF.GetSelectedValue<VersaoNFe>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "RetirarAcentos", ckbRetirarAcentos.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarWS", ckbSalvar.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "PathSalvar", txtLogs.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "PathSchemas", txtSchemaPath.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "IdCSC", txtIdCSC.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "CSC", txtCSC.Text);

            //Config Webservice
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "Timeout", nudTimeOut.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "Ambiente", rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "Visualizar", ckbVisualizar.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarWS", ckbSalvarSOAP.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "AjustaAguardaConsultaRet", ckbAjustarAut.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "AguardarConsultaRet", (int)nudAguardar.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "Tentativas", (int)nudTentativas.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "IntervaloTentativas", (int)nudIntervalos.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);

            //Config Certificado
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib", cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);

            //Config Arquivos
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarGer", ckbSalvarArqs.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SepararPorMes", ckbPastaMensal.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "AdicionarLiteral", ckbAdicionaLiteral.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "EmissaoPathNFe", ckbEmissaoPathNFe.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarArq", ckbSalvaPathEvento.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SepararPorCNPJ", ckbSepararPorCNPJ.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "SepararPorModelo", ckbSepararPorModelo.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "PathNFe", txtArqNFe.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "PathInu", txtArqInu.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.NFe, "PathEvento", txtArqEvento.Text);

            //Config Documento Auxiliar
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFE, "PathLogo", txtLogomarca.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFE, "Impressora", cmbDANFCeImpressora.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFE, "Copias", nudDANFCeCopias.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFE, "MostraPreview", chkNFCePreview.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFE, "MostraSetup", chkNFCeSetup.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "TipoRelatorioBobina", cmbTipoDANFCe.GetSelectedValue<TipoRelatorioBobina>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "TipoRelatorioEvento", cmbEventoDANFCe.GetSelectedValue<TipoRelatorioEvento>());
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "LarguraBobina", (int)nudDANFCeBobina.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "ImprimeDescAcrescItem", chkNFCeImprimeDescAcr.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "ImprimeItens", chkNFCeImprimeItens.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "ImprimeQRCodeLateral", chkNFCeQrCodeLateral.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "ImprimeLogoLateral", chkNFCeLogoLateral.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "ImprimeEmUmaLinha", chkNFCeImprimeUmaLinha.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "ImprimeEmDuasLinhas", chkNFCeImprimeDuasLinhas.Checked);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "MargemSuperior", (int)nudNFCeMSuperior.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "MargemInferior", (int)nudNFCeMInferior.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "MargemEsquerda", (int)nudNFCeMEsquerda.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "MargemDireita", (int)nudNFCeMDireita.Value);

            Caixa.NFe.ConfigGravar();

            #endregion NFe

            #region SAT

            //Carregando as config do PosPrinter
            Caixa.SAT.ConfigLer();

            Caixa.SAT.ConfigGravarValor(ACBrSessao.SAT, "NomeDLL", txtDllPath.Text);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SAT, "Modelo", cmbModeloSat.GetSelectedValue<SATModelo>());
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SAT, "CodigoDeAtivacao", txtAtivacao.Text);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfig, "infCFe_versaoDadosEnt", nunVersaoCFe.Value);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfig, "PaginaDeCodigo", nunPaginaCodigo.Value);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfig, "ide_CNPJ", txtSoftCNPJ.Text.OnlyNumbers());
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfig, "emit_cRegTrib", cmbSATRegTrib.SelectedIndex);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfig, "emit_cRegTribISSQN", cmbSATRegTribISSQN.SelectedIndex);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfig, "emit_indRatISSQN", cmbSATRatIISQN.SelectedIndex);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SAT, "SignAC", txtSignAc.Text);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SalvarCFe", chkSaveCFe.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SalvarEnvio", chkSaveEnvio.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SalvarCFeCanc", chkSaveCFeCanc.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SepararPorCNPJ", chkSepararCNPJ.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.SATConfigArquivos, "SepararPorDia", chkSepararData.Checked);

            //Impressão
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "Tipo", cmbImpressao.GetSelectedValue<TipoExtrato>());
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "Copias", nudCopias.Value);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "Impressora", cbbImpressora.Text);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "PathLogo", txtSatLogo.Text);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Extrato, "MargemSuperior", (int)nudSatMSuperior.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Extrato, "MargemInferior", (int)nudSatMInferior.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Extrato, "MargemEsquerda", (int)nudSatMEsquerda.Value);
            Caixa.NFe.ConfigGravarValor(ACBrSessao.Extrato, "MargemDireita", (int)nudSatMDireita.Value);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "MostraPreview", chkPreview.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "MostraSetup", chkSetup.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "ImprimeCodigoEan", chkUsaCodigoEanImpressao.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "ImprimeEmUmaLinha", chkImprimeEmUmaLinha.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "ImprimeLogoLateral", chkLogoLateral.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "ImprimeQRCodeLateral", chkQrCodeLateral.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "ExpandeLogoMarca", chkExpandirLogo.Checked);
            Caixa.SAT.ConfigGravarValor(ACBrSessao.Extrato, "LogoVisible", chkExibirLogo.Checked);

            Caixa.SAT.ConfigGravar();

            #endregion SAT

            #region Bal

            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL, "Modelo", cmbModelo.GetSelectedValue<ACBrBALModelo>());
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL, "Porta", cmbPorta.Text);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Baud", cmbBaud.SelectedItem);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Data", cmbDatabits.SelectedItem);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Parity", cmbParity.SelectedItem);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Stop", cmbStopbits.SelectedItem);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "HandShake", cmbHandshaking.SelectedItem);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "MaxBandwidth", nudMaxBand.Value);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "SendBytesCount", nudBytesCount.Value);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "SendBytesInterval", nudIntervalo.Value);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "SoftFlow", chkSoftFlow.Checked);
            Caixa.Bal.ConfigGravarValor(ACBrSessao.BAL_Device, "HardFlow", chkHardFlow.Checked);

            Caixa.Bal.ConfigGravar();

            #endregion Bal
        }

        #endregion Methods

        #region Event Handlers

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void btnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnArqNFe_Click(object sender, EventArgs e)
        {
            txtArqNFe.Text = Helpers.SelectFolder();
        }

        private void btnArqInu_Click(object sender, EventArgs e)
        {
            txtArqInu.Text = Helpers.SelectFolder();
        }

        private void btnArqEvento_Click(object sender, EventArgs e)
        {
            txtArqEvento.Text = Helpers.SelectFolder();
        }

        private void btnLogomarca_Click(object sender, EventArgs e)
        {
            txtLogomarca.Text = Helpers.OpenFile("Image files (*.bmp, *.jpeg, *.png) | *.bmp; *.jpeg; *.png");
        }

        private void btnSelDll_Click(object sender, EventArgs e)
        {
            txtDllPath.Text = Helpers.OpenFile("Biblioteca SAT (*.dll)|*.dll|Todo os Arquivos (*.*)|*.*");
        }

        private void btnSatLogo_Click(object sender, EventArgs e)
        {
            txtSatLogo.Text = Helpers.OpenFile("Image files (*.bmp, *.jpeg, *.png) | *.bmp; *.jpeg; *.png");
        }

        private void btnArqLog_Click(object sender, EventArgs e)
        {
            txtArqLog.Text = Helpers.OpenFile("Arquivo Log (*.log)|*.log|Todo os Arquivos (*.*)|*.*", checkFileExists: false);
        }

        private void btnImportar_Click(object sender, EventArgs e)
        {
            ImportarConfig();
        }

        private void btnExportar_Click(object sender, EventArgs e)
        {
            ExportarConfig();
        }

        private void btnCancelarConfig_Click(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();

            try
            {
                LoadConfig();
                ShowPDV();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnSaveConfig_Click(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();

            try
            {
                SaveConfig();
                ShowPDV();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnConfiguracoes_Click(object sender, EventArgs e)
        {
            if (!Caixa.Status.IsIn(StatusVenda.Finalizada, StatusVenda.Livre, StatusVenda.Cancelada)) return;

            SplashScreenManager.Show<FrmWait>();

            try
            {
                tbcViews.SelectedTab = tbpConfiguracoes;

                plnBobina.Enabled = false;
                plnBobina.Hide();
                plnBobina.Hide();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnIniciarVenda_Click(object sender, EventArgs e)
        {
            lstBobina.Items.Clear();
            Application.DoEvents();

            Caixa.IniciarVenda();
            btnIniciarVenda.Enabled = false;
            btnIdentificar.Enabled = true;
            btnVenderItem.Enabled = true;
            btnCancelaritem.Enabled = true;
            btnCancelar.Enabled = true;
            btnSubTotal.Enabled = true;
            btnPagamento.Enabled = false;
            btnEncerrar.Enabled = false;
        }

        private void btnIdentificar_Click(object sender, EventArgs e)
        {
            var documento = string.Empty;
            InputBox.Show("Identificação", "Informe do documento do cliente", ref documento);

            if (string.IsNullOrEmpty(documento)) return;

            var nome = string.Empty;
            InputBox.Show("Identificação", "Informe o nome do cliente", ref nome);

            Caixa.Identificar(documento, nome);
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            Caixa.CancelarVenda();

            btnIniciarVenda.Enabled = true;
            btnIdentificar.Enabled = false;
            btnVenderItem.Enabled = false;
            btnCancelaritem.Enabled = false;
            btnCancelar.Enabled = false;
            btnSubTotal.Enabled = false;
            btnPagamento.Enabled = false;
            btnEncerrar.Enabled = false;
        }

        private void btnVenderItem_Click(object sender, EventArgs e)
        {
            FrmVenderItem.VenderItem(this, Caixa);
        }

        private void btnCancelaritem_Click(object sender, EventArgs e)
        {
            if (dgvItensVenda.SelectedRows.Count < 1) return;

            var item = (RegistroVenda)dgvItensVenda.SelectedRows[0].DataBoundItem;
            Caixa.Cancelaritem(item);
        }

        private void btnSubTotal_Click(object sender, EventArgs e)
        {
            if (!Caixa.Itens.Any()) return;

            Caixa.SubTotalizar();

            btnIniciarVenda.Enabled = false;
            btnIdentificar.Enabled = true;
            btnVenderItem.Enabled = false;
            btnCancelaritem.Enabled = false;
            btnCancelar.Enabled = true;
            btnSubTotal.Enabled = false;
            btnPagamento.Enabled = true;
            btnEncerrar.Enabled = true;
        }

        private void btnPagamento_Click(object sender, EventArgs e)
        {
            FrmPagamento.EfetuarPagamento(this, Caixa);
        }

        private void btnEncerrar_Click(object sender, EventArgs e)
        {
            if (!Caixa.EncerraCupom()) return;

            btnIniciarVenda.Enabled = true;
            btnIdentificar.Enabled = false;
            btnVenderItem.Enabled = false;
            btnCancelaritem.Enabled = false;
            btnCancelar.Enabled = false;
            btnSubTotal.Enabled = false;
            btnPagamento.Enabled = false;
            btnEncerrar.Enabled = false;
        }

        private void btnImprimir_Click(object sender, EventArgs e)
        {
            var bobina = lstBobina.Items.Cast<string>().Concat(new[] { "</pular_linhas>", "</corte>" }).ToArray();
            Caixa.Imprimir(bobina);
        }

        private void dgvItensVenda_CellFormatting(object sender, DataGridViewCellFormattingEventArgs e)
        {
            if (e.RowIndex <= -1 || e.RowIndex >= Caixa.Itens.Length || e.ColumnIndex <= -1) return;

            var collumn = dgvItensVenda.Columns[e.ColumnIndex];
            switch (collumn)
            {
                case DataGridViewColumn _ when collumn.Name == dgcItem.Name:
                    e.Value = $"{Caixa.Itens[e.RowIndex].Item:000}";
                    break;

                case DataGridViewColumn _ when collumn.Name == dgcProduto.Name:
                    e.Value = Caixa.Itens[e.RowIndex].Produto.Descricao;
                    break;

                case DataGridViewColumn _ when collumn.Name == dgcUnidade.Name:
                    e.Value = Caixa.Itens[e.RowIndex].Produto.Unidade;
                    break;

                case DataGridViewColumn _ when collumn.Name == dgcQuantidade.Name:
                    e.Value = $"{Caixa.Itens[e.RowIndex].Quantidade:N3}";
                    break;

                case DataGridViewColumn _ when collumn.Name == dgcValor.Name:
                    e.Value = $"{Caixa.Itens[e.RowIndex].Produto.Valor:C2}";
                    break;

                case DataGridViewColumn _ when collumn.Name == dgcTotal.Name:
                    e.Value = $"{Caixa.Itens[e.RowIndex].ValorTotal:C2}";
                    break;
            }
        }

        #endregion Event Handlers
    }
}