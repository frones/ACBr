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
            cmbSATRegTrib.EnumDataSource(RegTrib.RTRegimeNormal);
            cmbSATRegTribISSQN.EnumDataSource(RegTribISSQN.RTISSNenhum);
            cmbSATRatIISQN.EnumDataSource(indRatISSQN.irNao);

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

            cmbPosBaud.EnumDataSource(SerialBaud.bd110);
            cmbPosDatabits.EnumDataSource(SerialDataBits.db5);
            cmbPosStopbits.EnumDataSource(SerialStopBytes.One);
            cmbPosParity.EnumDataSource(SerialParity.None);
            cmbPosHandshaking.EnumDataSource(SerialHandShake.Nenhum);

            //Balança
            cmbModelo.EnumDataSource(ACBrBALModelo.balNenhum);

            cmbPorta.Items.AddRange(SerialPort.GetPortNames());

            cmbBaud.EnumDataSource(SerialBaud.bd110);
            cmbDatabits.EnumDataSource(SerialDataBits.db5);
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
            ckbAtualizarXML.Checked = Caixa.NFe.Config.AtualizarXMLCancelado;
            ckbExibirErroSchema.Checked = Caixa.NFe.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = Caixa.NFe.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(Caixa.NFe.Config.FormaEmissao);
            cmbModeloDocumento.SetSelectedValue(Caixa.NFe.Config.ModeloDF);
            cmbVersaoDF.SetSelectedValue(Caixa.NFe.Config.VersaoDF);
            ckbRetirarAcentos.Checked = Caixa.NFe.Config.RetirarAcentos;
            ckbSalvar.Checked = Caixa.NFe.Config.SalvarWS;
            txtLogs.Text = Caixa.NFe.Config.PathSalvar;
            txtSchemaPath.Text = Caixa.NFe.Config.PathSchemas;
            txtIdCSC.Text = Caixa.NFe.Config.IdCSC;
            txtCSC.Text = Caixa.NFe.Config.CSC;

            //Config Webservice
            cmbUfDestino.SelectedItem = Caixa.NFe.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(Caixa.NFe.Config.SSLType);
            nudTimeOut.Value = Caixa.NFe.Config.Timeout;

            var ambiente = Caixa.NFe.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = Caixa.NFe.Config.Visualizar;
            ckbSalvarSOAP.Checked = Caixa.NFe.Config.SalvarWS;
            ckbAjustarAut.Checked = Caixa.NFe.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = Caixa.NFe.Config.AguardarConsultaRet;
            nudTentativas.Value = Caixa.NFe.Config.Tentativas;
            nudIntervalos.Value = Caixa.NFe.Config.IntervaloTentativas;
            txtProxyServidor.Text = Caixa.NFe.Config.Proxy.Servidor;
            nudProxyPorta.Text = Caixa.NFe.Config.Proxy.Porta;
            txtProxyUsuario.Text = Caixa.NFe.Config.Proxy.Usuario;
            txtProxySenha.Text = Caixa.NFe.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(Caixa.NFe.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(Caixa.NFe.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(Caixa.NFe.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = Caixa.NFe.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = Caixa.NFe.Config.DFe.Senha;
            txtCertNumero.Text = Caixa.NFe.Config.DFe.NumeroSerie;

            //Config Arquivos
            ckbSalvarArqs.Checked = Caixa.NFe.Config.SalvarGer;
            ckbPastaMensal.Checked = Caixa.NFe.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = Caixa.NFe.Config.AdicionarLiteral;
            ckbEmissaoPathNFe.Checked = Caixa.NFe.Config.EmissaoPathNFe;
            ckbSalvaPathEvento.Checked = Caixa.NFe.Config.SalvarArq;
            ckbSepararPorCNPJ.Checked = Caixa.NFe.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = Caixa.NFe.Config.SepararPorModelo;
            txtArqNFe.Text = Caixa.NFe.Config.PathNFe;
            txtArqInu.Text = Caixa.NFe.Config.PathInu;
            txtArqEvento.Text = Caixa.NFe.Config.PathEvento;

            //Config Documento Auxiliar
            txtLogomarca.Text = Caixa.NFe.Config.DANFe.PathLogo;
            nudDANFCeCopias.Value = Caixa.NFe.Config.DANFe.Copias;
            cmbDANFCeImpressora.Text = Caixa.NFe.Config.DANFe.Impressora;
            chkNFCePreview.Checked = Caixa.NFe.Config.DANFe.MostraPreview;
            chkNFCeSetup.Checked = Caixa.NFe.Config.DANFe.MostraSetup;
            cmbTipoDANFCe.SetSelectedValue(Caixa.NFe.Config.DANFe.NFCe.TipoRelatorioBobina);
            cmbEventoDANFCe.SetSelectedValue(Caixa.NFe.Config.DANFe.NFCe.TipoRelatorioEvento);
            nudDANFCeBobina.Value = Caixa.NFe.Config.DANFe.NFCe.LarguraBobina;
            chkNFCeImprimeDescAcr.Checked = Caixa.NFe.Config.DANFe.NFCe.ImprimeDescAcrescItem;
            chkNFCeImprimeItens.Checked = Caixa.NFe.Config.DANFe.NFCe.ImprimeItens;
            chkNFCeQrCodeLateral.Checked = Caixa.NFe.Config.DANFe.NFCe.ImprimeQRCodeLateral;
            chkNFCeLogoLateral.Checked = Caixa.NFe.Config.DANFe.NFCe.ImprimeLogoLateral;
            chkNFCeImprimeUmaLinha.Checked = Caixa.NFe.Config.DANFe.NFCe.ImprimeEmUmaLinha;
            chkNFCeImprimeDuasLinhas.Checked = Caixa.NFe.Config.DANFe.NFCe.ImprimeEmDuasLinhas;
            nudNFCeMSuperior.Value = Caixa.NFe.Config.DANFe.NFCe.MargemSuperior;
            nudNFCeMInferior.Value = Caixa.NFe.Config.DANFe.NFCe.MargemInferior;
            nudNFCeMEsquerda.Value = Caixa.NFe.Config.DANFe.NFCe.MargemEsquerda;
            nudNFCeMDireita.Value = Caixa.NFe.Config.DANFe.NFCe.MargemDireita;

            #endregion NFe

            #region SAT

            Caixa.SAT.ConfigLer();

            txtDllPath.Text = Caixa.SAT.Config.NomeDLL;
            cmbModeloSat.SetSelectedValue(Caixa.SAT.Config.Modelo);
            txtAtivacao.Text = Caixa.SAT.Config.CodigoDeAtivacao;
            nunVersaoCFe.Value = Caixa.SAT.Config.SatConfig.infCFe_versaoDadosEnt;
            nunPaginaCodigo.Value = Caixa.SAT.Config.SatConfig.PaginaDeCodigo;
            txtSoftCNPJ.Text = Caixa.SAT.Config.SatConfig.ide_CNPJ;
            cmbSATRegTrib.SetSelectedValue(Caixa.SAT.Config.SatConfig.emit_cRegTrib);
            cmbSATRegTribISSQN.SetSelectedValue(Caixa.SAT.Config.SatConfig.emit_cRegTribISSQN);
            cmbSATRatIISQN.SetSelectedValue(Caixa.SAT.Config.SatConfig.emit_indRatISSQN);
            txtSignAc.Text = Caixa.SAT.Config.SignAC;
            chkSaveCFe.Checked = Caixa.SAT.Config.Arquivos.SalvarCFe;
            chkSaveEnvio.Checked = Caixa.SAT.Config.Arquivos.SalvarEnvio;
            chkSaveCFeCanc.Checked = Caixa.SAT.Config.Arquivos.SalvarCFeCanc;
            chkSepararCNPJ.Checked = Caixa.SAT.Config.Arquivos.SepararPorCNPJ;
            chkSepararData.Checked = Caixa.SAT.Config.Arquivos.SepararPorDia;

            //Extrato
            cmbImpressao.SetSelectedValue(Caixa.SAT.Config.Extrato.Tipo);
            nudCopias.Value = Caixa.SAT.Config.Extrato.Copias;
            cbbImpressora.Text = Caixa.SAT.Config.Extrato.Impressora;
            txtSatLogo.Text = Caixa.SAT.Config.Extrato.PathLogo;
            nudSatMSuperior.Value = Caixa.SAT.Config.Extrato.MargemSuperior;
            nudSatMInferior.Value = Caixa.SAT.Config.Extrato.MargemInferior;
            nudSatMEsquerda.Value = Caixa.SAT.Config.Extrato.MargemEsquerda;
            nudSatMDireita.Value = Caixa.SAT.Config.Extrato.MargemDireita;
            chkPreview.Checked = Caixa.SAT.Config.Extrato.MostraPreview;
            chkSetup.Checked = Caixa.SAT.Config.Extrato.MostraSetup;
            chkUsaCodigoEanImpressao.Checked = Caixa.SAT.Config.Extrato.ImprimeCodigoEan;
            chkImprimeEmUmaLinha.Checked = Caixa.SAT.Config.Extrato.ImprimeEmUmaLinha;
            chkLogoLateral.Checked = Caixa.SAT.Config.Extrato.ImprimeLogoLateral;
            chkQrCodeLateral.Checked = Caixa.SAT.Config.Extrato.ImprimeQRCodeLateral;
            chkExpandirLogo.Checked = Caixa.SAT.Config.Extrato.ExpandeLogoMarca;
            chkExibirLogo.Checked = Caixa.SAT.Config.Extrato.LogoVisible;

            #endregion SAT

            #region PosPrinter

            Caixa.PosPrinter.ConfigLer();

            //PosPrinter
            txtSoftwareHouse.Text = Caixa.PosPrinter.Config.Sistema.Nome;
            txtWebsite.Text = Caixa.PosPrinter.Config.Emissor.WebSite;

            cbbModelo.SetSelectedValue(Caixa.PosPrinter.Config.Modelo);
            cbbPortas.Text = Caixa.PosPrinter.Config.Porta;
            nudColunas.Value = Caixa.PosPrinter.Config.ColunasFonteNormal;
            nudEspacos.Value = Caixa.PosPrinter.Config.EspacoEntreLinhas;
            nudBuffer.Value = Caixa.PosPrinter.Config.LinhasBuffer;
            nudLinhasPular.Value = Caixa.PosPrinter.Config.LinhasEntreCupons;
            cbxControlePorta.Checked = Caixa.PosPrinter.Config.ControlePorta;
            cbxCortarPapel.Checked = Caixa.PosPrinter.Config.CortaPapel;
            cbxTraduzirTags.Checked = Caixa.PosPrinter.Config.TraduzirTags;
            cbxIgnorarTags.Checked = Caixa.PosPrinter.Config.IgnorarTags;
            txtArqLog.Text = Caixa.PosPrinter.Config.ArqLog;
            cbbPaginaCodigo.SetSelectedValue(Caixa.PosPrinter.Config.PaginaDeCodigo);

            nudCodbarLargura.Value = Caixa.PosPrinter.Config.BarrasConfig.LarguraLinha;
            nudCodbarAltura.Value = Caixa.PosPrinter.Config.BarrasConfig.Altura;
            cbxCodbarExibeNumeros.Checked = Caixa.PosPrinter.Config.BarrasConfig.MostrarCodigo;

            nudQRTipo.Value = Caixa.PosPrinter.Config.QrCodeConfig.Tipo;
            nudQRLargura.Value = Caixa.PosPrinter.Config.QrCodeConfig.LarguraModulo;
            nudQRErrorLevel.Value = Caixa.PosPrinter.Config.QrCodeConfig.ErrorLevel;

            nudLogoKC1.Value = Caixa.PosPrinter.Config.LogoConfig.KeyCode1;
            nudLogoKC2.Value = Caixa.PosPrinter.Config.LogoConfig.KeyCode2;
            nudLogoFatorX.Value = Caixa.PosPrinter.Config.LogoConfig.FatorX;
            nudLogoFatorY.Value = Caixa.PosPrinter.Config.LogoConfig.FatorY;

            nudGVON.Value = Caixa.PosPrinter.Config.GavetaConfig.TempoON;
            nudGVOFF.Value = Caixa.PosPrinter.Config.GavetaConfig.TempoOFF;
            cbxGVInvertido.Checked = Caixa.PosPrinter.Config.GavetaConfig.SinalInvertido;

            cmbPosBaud.SetSelectedValue(Caixa.PosPrinter.Config.Device.Baud);
            cmbPosDatabits.SetSelectedValue(Caixa.PosPrinter.Config.Device.Data);
            cmbPosParity.SetSelectedValue(Caixa.PosPrinter.Config.Device.Parity);
            cmbPosStopbits.SetSelectedValue(Caixa.PosPrinter.Config.Device.Stop);
            cmbPosHandshaking.SetSelectedValue(Caixa.PosPrinter.Config.Device.HandShake);
            nudPosMaxBand.Value = Caixa.PosPrinter.Config.Device.MaxBandwidth;
            nudPosBytesCount.Value = Caixa.PosPrinter.Config.Device.SendBytesCount;
            nudPosIntervalo.Value = Caixa.PosPrinter.Config.Device.SendBytesInterval;
            chkPosSoftFlow.Checked = Caixa.PosPrinter.Config.Device.SoftFlow;
            chkPosHardFlow.Checked = Caixa.PosPrinter.Config.Device.HardFlow;

            #endregion PosPrinter

            #region Bal

            Caixa.Bal.ConfigLer();

            cmbModelo.SetSelectedValue(Caixa.Bal.Config.Modelo);
            cmbPorta.Text = Caixa.Bal.Config.Porta;
            cmbBaud.SetSelectedValue(Caixa.Bal.Config.Device.Baud);
            cmbDatabits.SetSelectedValue(Caixa.Bal.Config.Device.Data);
            cmbParity.SetSelectedValue(Caixa.Bal.Config.Device.Parity);
            cmbStopbits.SetSelectedValue(Caixa.Bal.Config.Device.Stop);
            cmbHandshaking.SetSelectedValue(Caixa.Bal.Config.Device.HandShake);
            nudMaxBand.Value = Caixa.Bal.Config.Device.MaxBandwidth;
            nudBytesCount.Value = Caixa.Bal.Config.Device.SendBytesCount;
            nudIntervalo.Value = Caixa.Bal.Config.Device.SendBytesInterval;
            chkSoftFlow.Checked = Caixa.Bal.Config.Device.SoftFlow;
            chkHardFlow.Checked = Caixa.Bal.Config.Device.HardFlow;

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
            Caixa.PosPrinter.Config.Sistema.Nome = txtSoftwareHouse.Text;
            Caixa.PosPrinter.Config.Emissor.WebSite = txtWebsite.Text;

            Caixa.PosPrinter.Config.Modelo = cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>();
            Caixa.PosPrinter.Config.Porta = cbbPortas.Text;
            Caixa.PosPrinter.Config.ColunasFonteNormal = (int)nudColunas.Value;
            Caixa.PosPrinter.Config.EspacoEntreLinhas = (int)nudEspacos.Value;
            Caixa.PosPrinter.Config.LinhasBuffer = (int)nudBuffer.Value;
            Caixa.PosPrinter.Config.LinhasEntreCupons = (int)nudLinhasPular.Value;
            Caixa.PosPrinter.Config.ControlePorta = cbxControlePorta.Checked;
            Caixa.PosPrinter.Config.CortaPapel = cbxCortarPapel.Checked;
            Caixa.PosPrinter.Config.TraduzirTags = cbxTraduzirTags.Checked;
            Caixa.PosPrinter.Config.IgnorarTags = cbxIgnorarTags.Checked;
            Caixa.PosPrinter.Config.ArqLog = txtArqLog.Text;
            Caixa.PosPrinter.Config.PaginaDeCodigo = cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>();

            Caixa.PosPrinter.Config.BarrasConfig.LarguraLinha = (int)nudCodbarLargura.Value;
            Caixa.PosPrinter.Config.BarrasConfig.Altura = (int)nudCodbarAltura.Value;
            Caixa.PosPrinter.Config.BarrasConfig.MostrarCodigo = cbxCodbarExibeNumeros.Checked;

            Caixa.PosPrinter.Config.QrCodeConfig.Tipo = (int)nudQRTipo.Value;
            Caixa.PosPrinter.Config.QrCodeConfig.LarguraModulo = (int)nudQRLargura.Value;
            Caixa.PosPrinter.Config.QrCodeConfig.ErrorLevel = (int)nudQRErrorLevel.Value;

            Caixa.PosPrinter.Config.LogoConfig.KeyCode1 = (byte)nudLogoKC1.Value;
            Caixa.PosPrinter.Config.LogoConfig.KeyCode2 = (byte)nudLogoKC2.Value;
            Caixa.PosPrinter.Config.LogoConfig.FatorX = (byte)nudLogoFatorX.Value;
            Caixa.PosPrinter.Config.LogoConfig.FatorY = (byte)nudLogoFatorY.Value;

            Caixa.PosPrinter.Config.GavetaConfig.TempoON = (byte)nudGVON.Value;
            Caixa.PosPrinter.Config.GavetaConfig.TempoOFF = (byte)nudGVOFF.Value;
            Caixa.PosPrinter.Config.GavetaConfig.SinalInvertido = cbxGVInvertido.Checked;

            Caixa.PosPrinter.Config.Device.Baud = cmbPosBaud.GetSelectedValue<SerialBaud>();
            Caixa.PosPrinter.Config.Device.Data = cmbPosDatabits.GetSelectedValue<SerialDataBits>();
            Caixa.PosPrinter.Config.Device.Parity = cmbPosParity.GetSelectedValue<SerialParity>();
            Caixa.PosPrinter.Config.Device.Stop = cmbPosStopbits.GetSelectedValue<SerialStopBytes>();
            Caixa.PosPrinter.Config.Device.HandShake = cmbPosHandshaking.GetSelectedValue<SerialHandShake>();
            Caixa.PosPrinter.Config.Device.MaxBandwidth = (int)nudPosMaxBand.Value;
            Caixa.PosPrinter.Config.Device.SendBytesCount = (int)nudPosBytesCount.Value;
            Caixa.PosPrinter.Config.Device.SendBytesInterval = (int)nudPosIntervalo.Value;
            Caixa.PosPrinter.Config.Device.SoftFlow = chkPosSoftFlow.Checked;
            Caixa.PosPrinter.Config.Device.HardFlow = chkPosHardFlow.Checked;

            Caixa.PosPrinter.ConfigGravar();

            #endregion PosPrinter

            #region NFe

            //Carregando a config do PosPrinter
            Caixa.NFe.ConfigLer();

            //Config Geral
            Caixa.NFe.Config.AtualizarXMLCancelado = ckbAtualizarXML.Checked;
            Caixa.NFe.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
            Caixa.NFe.Config.FormatoAlerta = txtFormatoAlerta.Text;
            Caixa.NFe.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
            Caixa.NFe.Config.ModeloDF = cmbModeloDocumento.GetSelectedValue<ModeloNFe>();
            Caixa.NFe.Config.VersaoDF = cmbVersaoDF.GetSelectedValue<VersaoNFe>();
            Caixa.NFe.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
            Caixa.NFe.Config.SalvarWS = ckbSalvar.Checked;
            Caixa.NFe.Config.PathSalvar = txtLogs.Text;
            Caixa.NFe.Config.PathSchemas = txtSchemaPath.Text;
            Caixa.NFe.Config.IdCSC = txtIdCSC.Text;
            Caixa.NFe.Config.CSC = txtCSC.Text;

            //Config Webservice
            Caixa.NFe.Config.DFe.UF = cmbUfDestino.Text;
            Caixa.NFe.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
            Caixa.NFe.Config.Timeout = (int)nudTimeOut.Value;
            Caixa.NFe.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
            Caixa.NFe.Config.Visualizar = ckbVisualizar.Checked;
            Caixa.NFe.Config.SalvarWS = ckbSalvarSOAP.Checked;
            Caixa.NFe.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
            Caixa.NFe.Config.AguardarConsultaRet = (int)nudAguardar.Value;
            Caixa.NFe.Config.Tentativas = (int)nudTentativas.Value;
            Caixa.NFe.Config.IntervaloTentativas = (int)nudIntervalos.Value;
            Caixa.NFe.Config.Proxy.Servidor = txtProxyServidor.Text;
            Caixa.NFe.Config.Proxy.Porta = nudProxyPorta.Text;
            Caixa.NFe.Config.Proxy.Usuario = txtProxyUsuario.Text;
            Caixa.NFe.Config.Proxy.Senha = txtProxySenha.Text;

            //Config Certificado
            Caixa.NFe.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
            Caixa.NFe.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
            Caixa.NFe.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
            Caixa.NFe.Config.DFe.ArquivoPFX = txtCertPath.Text;
            Caixa.NFe.Config.DFe.Senha = txtCertPassword.Text;
            Caixa.NFe.Config.DFe.NumeroSerie = txtCertNumero.Text;

            //Config Arquivos
            Caixa.NFe.Config.SalvarGer = ckbSalvarArqs.Checked;
            Caixa.NFe.Config.SepararPorMes = ckbPastaMensal.Checked;
            Caixa.NFe.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
            Caixa.NFe.Config.EmissaoPathNFe = ckbEmissaoPathNFe.Checked;
            Caixa.NFe.Config.SalvarArq = ckbSalvaPathEvento.Checked;
            Caixa.NFe.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
            Caixa.NFe.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
            Caixa.NFe.Config.PathNFe = txtArqNFe.Text;
            Caixa.NFe.Config.PathInu = txtArqInu.Text;
            Caixa.NFe.Config.PathEvento = txtArqEvento.Text;

            //Config Documento Auxiliar
            Caixa.NFe.Config.DANFe.PathLogo = txtLogomarca.Text;
            Caixa.NFe.Config.DANFe.Impressora = cmbDANFCeImpressora.Text;
            Caixa.NFe.Config.DANFe.Copias = (int)nudDANFCeCopias.Value;
            Caixa.NFe.Config.DANFe.MostraPreview = chkNFCePreview.Checked;
            Caixa.NFe.Config.DANFe.MostraSetup = chkNFCeSetup.Checked;
            Caixa.NFe.Config.DANFe.NFCe.TipoRelatorioBobina = cmbTipoDANFCe.GetSelectedValue<TipoRelatorioBobina>();
            Caixa.NFe.Config.DANFe.NFCe.TipoRelatorioEvento = cmbEventoDANFCe.GetSelectedValue<TipoRelatorioEvento>();
            Caixa.NFe.Config.DANFe.NFCe.LarguraBobina = (int)nudDANFCeBobina.Value;
            Caixa.NFe.Config.DANFe.NFCe.ImprimeDescAcrescItem = chkNFCeImprimeDescAcr.Checked;
            Caixa.NFe.Config.DANFe.NFCe.ImprimeItens = chkNFCeImprimeItens.Checked;
            Caixa.NFe.Config.DANFe.NFCe.ImprimeQRCodeLateral = chkNFCeQrCodeLateral.Checked;
            Caixa.NFe.Config.DANFe.NFCe.ImprimeLogoLateral = chkNFCeLogoLateral.Checked;
            Caixa.NFe.Config.DANFe.NFCe.ImprimeEmUmaLinha = chkNFCeImprimeUmaLinha.Checked;
            Caixa.NFe.Config.DANFe.NFCe.ImprimeEmDuasLinhas = chkNFCeImprimeDuasLinhas.Checked;
            Caixa.NFe.Config.DANFe.NFCe.MargemSuperior = (int)nudNFCeMSuperior.Value;
            Caixa.NFe.Config.DANFe.NFCe.MargemInferior = (int)nudNFCeMInferior.Value;
            Caixa.NFe.Config.DANFe.NFCe.MargemEsquerda = (int)nudNFCeMEsquerda.Value;
            Caixa.NFe.Config.DANFe.NFCe.MargemDireita = (int)nudNFCeMDireita.Value;

            Caixa.NFe.ConfigGravar();

            #endregion NFe

            #region SAT

            //Carregando as config do PosPrinter
            Caixa.SAT.ConfigLer();

            Caixa.SAT.Config.NomeDLL = txtDllPath.Text;
            Caixa.SAT.Config.Modelo = cmbModeloSat.GetSelectedValue<SATModelo>();
            Caixa.SAT.Config.CodigoDeAtivacao = txtAtivacao.Text;
            Caixa.SAT.Config.SatConfig.infCFe_versaoDadosEnt = nunVersaoCFe.Value;
            Caixa.SAT.Config.SatConfig.PaginaDeCodigo = (ushort)nunPaginaCodigo.Value;
            Caixa.SAT.Config.SatConfig.ide_CNPJ = txtSoftCNPJ.Text.OnlyNumbers();
            Caixa.SAT.Config.SatConfig.emit_cRegTrib = cmbSATRegTrib.GetSelectedValue<RegTrib>();
            Caixa.SAT.Config.SatConfig.emit_cRegTribISSQN = cmbSATRegTribISSQN.GetSelectedValue<RegTribISSQN>();
            Caixa.SAT.Config.SatConfig.emit_indRatISSQN = cmbSATRatIISQN.GetSelectedValue<indRatISSQN>();
            Caixa.SAT.Config.SignAC = txtSignAc.Text;
            Caixa.SAT.Config.Arquivos.SalvarCFe = chkSaveCFe.Checked;
            Caixa.SAT.Config.Arquivos.SalvarEnvio = chkSaveEnvio.Checked;
            Caixa.SAT.Config.Arquivos.SalvarCFeCanc = chkSaveCFeCanc.Checked;
            Caixa.SAT.Config.Arquivos.SepararPorCNPJ = chkSepararCNPJ.Checked;
            Caixa.SAT.Config.Arquivos.SepararPorDia = chkSepararData.Checked;

            //Impressão
            Caixa.SAT.Config.Extrato.Tipo = cmbImpressao.GetSelectedValue<TipoExtrato>();
            Caixa.SAT.Config.Extrato.Copias = (int)nudCopias.Value;
            Caixa.SAT.Config.Extrato.Impressora = cbbImpressora.Text;
            Caixa.SAT.Config.Extrato.PathLogo = txtSatLogo.Text;
            Caixa.SAT.Config.Extrato.MargemSuperior = (int)nudSatMSuperior.Value;
            Caixa.SAT.Config.Extrato.MargemInferior = (int)nudSatMInferior.Value;
            Caixa.SAT.Config.Extrato.MargemEsquerda = (int)nudSatMEsquerda.Value;
            Caixa.SAT.Config.Extrato.MargemDireita = (int)nudSatMDireita.Value;
            Caixa.SAT.Config.Extrato.MostraPreview = chkPreview.Checked;
            Caixa.SAT.Config.Extrato.MostraSetup = chkSetup.Checked;
            Caixa.SAT.Config.Extrato.ImprimeCodigoEan = chkUsaCodigoEanImpressao.Checked;
            Caixa.SAT.Config.Extrato.ImprimeEmUmaLinha = chkImprimeEmUmaLinha.Checked;
            Caixa.SAT.Config.Extrato.ImprimeLogoLateral = chkLogoLateral.Checked;
            Caixa.SAT.Config.Extrato.ImprimeQRCodeLateral = chkQrCodeLateral.Checked;
            Caixa.SAT.Config.Extrato.ExpandeLogoMarca = chkExpandirLogo.Checked;
            Caixa.SAT.Config.Extrato.LogoVisible = chkExibirLogo.Checked;

            Caixa.SAT.ConfigGravar();

            #endregion SAT

            #region Bal

            Caixa.Bal.Config.Modelo = cmbModelo.GetSelectedValue<ACBrBALModelo>();
            Caixa.Bal.Config.Porta = cmbPorta.Text;
            Caixa.Bal.Config.Device.Baud = cmbBaud.GetSelectedValue<SerialBaud>();
            Caixa.Bal.Config.Device.Data = cmbDatabits.GetSelectedValue<SerialDataBits>();
            Caixa.Bal.Config.Device.Parity = cmbParity.GetSelectedValue<SerialParity>();
            Caixa.Bal.Config.Device.Stop = cmbStopbits.GetSelectedValue<SerialStopBytes>();
            Caixa.Bal.Config.Device.HandShake = cmbHandshaking.GetSelectedValue<SerialHandShake>();
            Caixa.Bal.Config.Device.MaxBandwidth = (int)nudMaxBand.Value;
            Caixa.Bal.Config.Device.SendBytesCount = (int)nudBytesCount.Value;
            Caixa.Bal.Config.Device.SendBytesInterval = (int)nudIntervalo.Value;
            Caixa.Bal.Config.Device.SoftFlow = chkSoftFlow.Checked;
            Caixa.Bal.Config.Device.HardFlow = chkHardFlow.Checked;

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