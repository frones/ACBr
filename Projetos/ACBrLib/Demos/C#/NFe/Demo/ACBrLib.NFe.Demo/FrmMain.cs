using System;
using System.Collections.Generic;
using System.Drawing.Printing;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Extensions;
using ACBrLib.Core.NFe;
using ACBrLib.Core.PosPrinter;

namespace ACBrLib.NFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrNFe ACBrNFe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            //ACBrNFe = new ACBrNFe(File.ReadAllText("ACBrLib2.ini"));
            //ACBrNFe = new ACBrNFe("[Memory]");
            ACBrNFe = new ACBrNFe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            ACBrNFe.Dispose();
            ACBrNFe = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando...");

            try
            {
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbModeloDocumento.EnumDataSource(ModeloNFe.moNFe);
                cmbVersaoDF.EnumDataSource(VersaoNFe.ve400);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);

                cbbPortas.Items.AddRange(SerialPort.GetPortNames());
                cbbPortas.Items.Add(@"\\localhost\Epson");
                cbbPortas.Items.Add(@"c:\temp\posprinter.txt");

                cbbPortas.SelectedIndex = cbbPortas.Items.Count - 1;

                cbbPortas.Items.Add("TCP:192.168.0.31:9100");

                foreach (string printer in PrinterSettings.InstalledPrinters)
                    cbbPortas.Items.Add($"RAW:{printer}");

                cbbModelo.EnumDataSource(ACBrPosPrinterModelo.ppTexto);
                cbbPaginaCodigo.EnumDataSource(PosPaginaCodigo.pc850);

                // Altera as config de log
                ACBrNFe.Config.Principal.LogNivel = NivelLog.logCompleto;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrNFe.Config.Principal.LogPath = logPath;
                ACBrNFe.ConfigGravar();

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        #endregion Constructors

        #region Methods

        private string AlimentarDados()
        {
            var notaFiscal = new NotaFiscal();

            // infNFe
            notaFiscal.InfNFe.Versao = "4.0";

            // Identificação
            notaFiscal.Identificacao.cUF = 35;
            notaFiscal.Identificacao.cNF = 19456927;
            notaFiscal.Identificacao.natOp = "VENDA";
            notaFiscal.Identificacao.indPag = IndicadorPagamento.ipVista;
            notaFiscal.Identificacao.modelo = cmbModeloDocumento.GetSelectedValue<ModeloNFe>();
            notaFiscal.Identificacao.Serie = "1";
            notaFiscal.Identificacao.nNF = 1;
            notaFiscal.Identificacao.dhEmi = DateTime.Now;
            notaFiscal.Identificacao.dhSaiEnt = DateTime.Now;
            notaFiscal.Identificacao.tpNF = TipoNFe.tnSaida;
            notaFiscal.Identificacao.idDest = DestinoOperacao.doInterna;
            notaFiscal.Identificacao.tpAmb = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
            notaFiscal.Identificacao.tpImp = TipoDANFE.tiRetrato;
            notaFiscal.Identificacao.tpEmis = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
            notaFiscal.Identificacao.finNFe = FinalidadeNFe.fnNormal;
            notaFiscal.Identificacao.indFinal = ConsumidorFinal.cfConsumidorFinal;
            notaFiscal.Identificacao.indPres = PresencaComprador.pcPresencial;
            notaFiscal.Identificacao.procEmi = ProcessoEmissao.peAplicativoContribuinte;
            notaFiscal.Identificacao.indIntermed = IndIntermed.iiSemOperacao;
            notaFiscal.Identificacao.verProc = "ACBrNFe";
            //Reforma tributária
            //notaFiscal.Identificacao.cMunFGIBS = 3554003;
            //notaFiscal.Identificacao.tpNFCredito = TipoNFeCredito.tcNenhum;
            //notaFiscal.Identificacao.tpNFDebito = TipoNFeDebito.tdAnulacao;
            //notaFiscal.Identificacao.tpEnteGov = TipoEnteGov.tcgEstados;
            //notaFiscal.Identificacao.pRedutor = 2.5m;
            //--

            // Emitente
            notaFiscal.Emitente.CRT = CRT.crtRegimeNormal;
            notaFiscal.Emitente.CNPJCPF = "99999999999999";
            notaFiscal.Emitente.xNome = "Razao Social Emitente";
            notaFiscal.Emitente.xFant = "Fantasia";
            notaFiscal.Emitente.IE = "111111111111";
            notaFiscal.Emitente.IEST = "";
            notaFiscal.Emitente.IM = "";
            notaFiscal.Emitente.CNAE = "";
            notaFiscal.Emitente.xLgr = "Logradouro, do emitente";
            notaFiscal.Emitente.nro = "S/N";
            notaFiscal.Emitente.xCpl = "";
            notaFiscal.Emitente.xBairro = "Bairro";
            notaFiscal.Emitente.cMun = 3550308;
            notaFiscal.Emitente.xMun = "São Paulo";
            notaFiscal.Emitente.cUF = "35";
            notaFiscal.Emitente.UF = "SP";
            notaFiscal.Emitente.CEP = "11111111";
            notaFiscal.Emitente.cPais = 1058;
            notaFiscal.Emitente.xPais = "BRASIL";
            notaFiscal.Emitente.Fone = "11111111111111";

            // Destinatário
            // Observação ----> Obrigatório o preenchimento para o modelo 55.
            notaFiscal.Destinatario.CNPJCPF = "48674547000107";
            notaFiscal.Destinatario.xNome = "Razao Social Destinatário";
            notaFiscal.Destinatario.indIEDest = IndicadorIE.inNaoContribuinte;
            notaFiscal.Destinatario.IE = "111111111111";
            notaFiscal.Destinatario.ISUF = "";
            notaFiscal.Destinatario.Email = "emaildest@mail.com.br";
            notaFiscal.Destinatario.xLgr = "Logradouro, do destinatario";
            notaFiscal.Destinatario.nro = "123";
            notaFiscal.Destinatario.xCpl = "";
            notaFiscal.Destinatario.xBairro = "Centro";
            notaFiscal.Destinatario.cMun = 3550308;
            notaFiscal.Destinatario.xMun = "São Paulo";
            notaFiscal.Destinatario.UF = "SP";
            notaFiscal.Destinatario.CEP = "00000000";
            notaFiscal.Destinatario.cPais = 1058;
            notaFiscal.Destinatario.xPais = "BRASIL";
            notaFiscal.Destinatario.Fone = "11111111111111";

            // Produto
            var produto = new ProdutoNFe();
            produto.nItem = 1;
            produto.cProd = "123456";
            produto.cEAN = "7896523206646";
            produto.xProd = "Descrição do Produto";
            produto.indTot = IndicadorTotal.itSomaTotalNFe;
            produto.NCM = "85395200";
            produto.CEST = "1111111";
            produto.CFOP = "5101";
            produto.uCom = "UN";
            produto.qCom = 1;
            produto.vUnCom = 100;
            produto.cEANTrib = "7896523206646";
            produto.cBarraTrib = "ABC123456";
            produto.uTrib = "UN";
            produto.qTrib = 1;
            produto.vUnTrib = 100;

            // Calculando o total do produto corretamente
            decimal totalProdutos = produto.vUnCom * produto.qCom;
            produto.vProd = totalProdutos;

            //Reforma tributária
            //produto.vItem = 100;
            //--
            //Reforma tributária
            //produto.DFeReferenciado.nItem = 100;
            //produto.DFeReferenciado.chaveAcesso = "35250518760540000139550010000000011374749890";
            //--

            // Tributação
            produto.ICMS.CSOSN = CSOSNIcms.csosnVazio;
            produto.ICMS.CST = CSTIcms.cst00;
            produto.ICMS.modBC = DeterminacaoBaseIcms.dbiValorOperacao;  // Base de cálculo do ICMS
            produto.ICMS.vBC = produto.vProd;  // Base de cálculo do ICMS
            produto.ICMS.pICMS = 18;  // Alíquota de ICMS
            produto.ICMS.vICMS = produto.ICMS.vBC * produto.ICMS.pICMS / 100;

            // Cálculos de PIS e COFINS
            produto.PIS.CST = CSTPIS.pis04;
            produto.COFINS.CST = CSTCofins.cof04;

            // Campos adicionais
            produto.vFrete = 0;
            produto.vSeg = 0;
            produto.vDesc = 0;
            produto.vOutro = 0;

            produto.infAdProd = "Informação adicional do produto";

            //Reforma tributária
            //produto.IS.CSTIS = 200;
            //produto.IS.cClassTribIS = 200010;
            //produto.IS.vBCIS = 100;
            //produto.IS.pIS = 5;
            //produto.IS.pISEspec = 5;
            //produto.IS.uTrib = "UNIDAD";
            //produto.IS.qTrib = 10;
            //produto.IS.vIS = 100;

            //produto.IBSCBS.CST = CSTIBSCBS.cst800;
            //produto.IBSCBS.cClassTrib = cClassTribIBSCBS.cct800001;

            //produto.IBSCBS.gIBSCBS.vBC = 100;

            //produto.IBSCBS.gIBSCBS.gIBSUF.pIBSUF = 5;
            //produto.IBSCBS.gIBSCBS.gIBSUF.vIBSUF = 100;
            //produto.IBSCBS.gIBSCBS.gIBSUF.pDif = 5;
            //produto.IBSCBS.gIBSCBS.gIBSUF.vDif = 100;
            //produto.IBSCBS.gIBSCBS.gIBSUF.vDevTrib = 100;
            //produto.IBSCBS.gIBSCBS.gIBSUF.pRedAliq = 5;
            //produto.IBSCBS.gIBSCBS.gIBSUF.pAliqEfet = 5;

            //produto.IBSCBS.gIBSCBS.gIBSMun.pIBSMun = 5;
            //produto.IBSCBS.gIBSCBS.gIBSMun.vIBSMun = 100;
            //produto.IBSCBS.gIBSCBS.gIBSMun.pDif = 5;
            //produto.IBSCBS.gIBSCBS.gIBSMun.vDif = 100;
            //produto.IBSCBS.gIBSCBS.gIBSMun.vDevTrib = 100;
            //produto.IBSCBS.gIBSCBS.gIBSMun.pRedAliq = 5;
            //produto.IBSCBS.gIBSCBS.gIBSMun.pAliqEfet = 5;

            //produto.IBSCBS.gIBSCBS.gCBS.pCBS = 5;
            //produto.IBSCBS.gIBSCBS.gCBS.vCBS = 100;
            //produto.IBSCBS.gIBSCBS.gCBS.pDif = 5;
            //produto.IBSCBS.gIBSCBS.gCBS.vDif = 100;
            //produto.IBSCBS.gIBSCBS.gCBS.vDevTrib = 100;
            //produto.IBSCBS.gIBSCBS.gCBS.pRedAliq = 5;
            //produto.IBSCBS.gIBSCBS.gCBS.pAliqEfet = 5;

            //produto.IBSCBS.gIBSCBS.gTribRegular.CSTReg = CSTIBSCBS.cst000;
            //produto.IBSCBS.gIBSCBS.gTribRegular.cClassTribReg = cClassTribIBSCBS.cct000001;
            //produto.IBSCBS.gIBSCBS.gTribRegular.pAliqEfetRegIBSUF = 5;
            //produto.IBSCBS.gIBSCBS.gTribRegular.vTribRegIBSUF = 100;
            //produto.IBSCBS.gIBSCBS.gTribRegular.pAliqEfetRegIBSMun = 5;
            //produto.IBSCBS.gIBSCBS.gTribRegular.vTribRegIBSMun = 100;
            //produto.IBSCBS.gIBSCBS.gTribRegular.pAliqEfetRegCBS = 5;
            //produto.IBSCBS.gIBSCBS.gTribRegular.vTribRegCBS = 100;

            //produto.IBSCBS.gIBSCBS.gIBSCredPres.cCredPres = 1;
            //produto.IBSCBS.gIBSCBS.gIBSCredPres.pCredPres = 5;
            //produto.IBSCBS.gIBSCBS.gIBSCredPres.vCredPres = 100;
            ////produto.IBSCBS.gIBSCBS.gIBSCredPres.vCredPresCondSus = 100;

            //produto.IBSCBS.gIBSCBS.gCBSCredPres.cCredPres = 1;
            //produto.IBSCBS.gIBSCBS.gCBSCredPres.pCredPres = 5;
            //produto.IBSCBS.gIBSCBS.gCBSCredPres.vCredPres = 100;
            ////produto.IBSCBS.gIBSCBS.gCBSCredPres.vCredPresCondSus = 100;

            //produto.IBSCBS.gIBSCBSMono.qBCMono = 1;
            //produto.IBSCBS.gIBSCBSMono.adRemIBS = 5;
            //produto.IBSCBS.gIBSCBSMono.adRemCBS = 5;
            //produto.IBSCBS.gIBSCBSMono.vIBSMono = 100;
            //produto.IBSCBS.gIBSCBSMono.vCBSMono = 100;

            //produto.IBSCBS.gIBSCBSMono.qBCMonoReten = 1;
            //produto.IBSCBS.gIBSCBSMono.adRemCBSReten = 5;
            //produto.IBSCBS.gIBSCBSMono.vIBSMonoReten = 100;
            //produto.IBSCBS.gIBSCBSMono.vCBSMonoReten = 100;

            //produto.IBSCBS.gIBSCBSMono.qBCMonoRet = 1;
            //produto.IBSCBS.gIBSCBSMono.adRemIBSRet = 5;
            //produto.IBSCBS.gIBSCBSMono.vIBSMonoRet = 100;
            //produto.IBSCBS.gIBSCBSMono.vCBSMonoRet = 100;

            //produto.IBSCBS.gIBSCBSMono.pDifIBS = 5;
            //produto.IBSCBS.gIBSCBSMono.vIBSMonoDif = 100;
            //produto.IBSCBS.gIBSCBSMono.pDifCBS = 5;
            //produto.IBSCBS.gIBSCBSMono.vCBSMonoDif = 100;

            //produto.IBSCBS.gIBSCBSMono.vTotIBSMonoItem = 100;
            //produto.IBSCBS.gIBSCBSMono.vTotCBSMonoItem = 100;

            //produto.IBSCBS.gTransfCred.vCBS = 100;
            //produto.IBSCBS.gTransfCred.vIBS = 100;
            //--

            notaFiscal.Produtos.Add(produto);

            notaFiscal.Total.vBC = produto.vProd;
            notaFiscal.Total.vICMS = (decimal)produto.ICMS.vICMS;
            notaFiscal.Total.vProd = produto.vProd;
            notaFiscal.Total.vNF = produto.vProd;

            //Reforma tributária
            //notaFiscal.Total.ISTot.vIS = 100;

            //notaFiscal.Total.IBSCBSTot.vBCIBSCBS = 100;

            //notaFiscal.Total.IBSCBSTot.gIBS.vIBS = 100;
            //notaFiscal.Total.IBSCBSTot.gIBS.vCredPres = 100;
            //notaFiscal.Total.IBSCBSTot.gIBS.vCredPresCondSus = 100;

            //notaFiscal.Total.IBSCBSTot.gIBS.gIBSUF.vDif = 100;
            //notaFiscal.Total.IBSCBSTot.gIBS.gIBSUF.vDevTrib = 100;
            //notaFiscal.Total.IBSCBSTot.gIBS.gIBSUF.vIBSUF = 100;

            //notaFiscal.Total.IBSCBSTot.gIBS.gIBSMun.vDif = 100;
            //notaFiscal.Total.IBSCBSTot.gIBS.gIBSMun.vDevTrib = 100;
            //notaFiscal.Total.IBSCBSTot.gIBS.gIBSMun.vIBSMun = 100;

            //notaFiscal.Total.IBSCBSTot.gCBS.vDif = 100;
            //notaFiscal.Total.IBSCBSTot.gCBS.vDevTrib = 100;
            //notaFiscal.Total.IBSCBSTot.gCBS.vCBS = 100;
            //notaFiscal.Total.IBSCBSTot.gCBS.vCredPres = 100;
            //notaFiscal.Total.IBSCBSTot.gCBS.vCredPresCondSus = 100;

            //notaFiscal.Total.IBSCBSTot.gMono.vIBSMono = 100;
            //notaFiscal.Total.IBSCBSTot.gMono.vCBSMono = 100;
            //notaFiscal.Total.IBSCBSTot.gMono.vIBSMonoReten = 100;
            //notaFiscal.Total.IBSCBSTot.gMono.vCBSMonoReten = 100;
            //notaFiscal.Total.IBSCBSTot.gMono.vIBSMonoRet = 100;
            //notaFiscal.Total.IBSCBSTot.gMono.vCBSMonoRet = 100;

            //notaFiscal.Total.vNFTot = 100;
            //--

            PagamentoNFe pagtoDinheiro = new PagamentoNFe();
            pagtoDinheiro.tpIntegra = TpIntegra.tiNaoInformado;
            pagtoDinheiro.indPag = IndicadorPagamento.ipVista;
            pagtoDinheiro.tPag = FormaPagamento.fpDinheiro;
            pagtoDinheiro.vPag = 100;
            pagtoDinheiro.dPag = DateTime.Now;
            notaFiscal.Pagamentos.Add(pagtoDinheiro);

            //Exemplos de integração dos meios de pagamento aos documentos fiscais eletrônicos mencionados na aula
            //https://acbr.nutror.com/curso/8d575bd8a7c0ac0fda312f9b12b1eb521e606446/aula/9286660
            //Disponível no curso Integração dos Meios de Pagamento aos Documentos Fiscais Eletrônicos
            //-->Exemplo pagamento cartão débito/crédito para o Mato Grosso.
            
            //PagamentoNFe pagtoCartaoMT = new PagamentoNFe();
            //pagtoCartaoMT.tPag = FormaPagamento.fpCartaoCredito;

            //pagtoCartaoMT.vPag = 100;
            //pagtoCartaoMT.tpIntegra = TpIntegra.tiPagIntegrado;
            //pagtoCartaoMT.CNPJ = "99999999999999";
            //pagtoCartaoMT.cAut = "123456789012345678901234567890";
            //pagtoCartaoMT.CNPJReceb = "123456789101234";
            //pagtoCartaoMT.idTermPag = "12345678901234567890";

            //notaFiscal.Pagamentos.Add(pagtoCartaoMT);

            //-->Exemplo pagamento PIX para o Mato Grosso.
            //PagamentoNFe pagtoPIXMT = new PagamentoNFe();
            
            //pagtoPIXMT.tPag = FormaPagamento.fpPagamentoInstantaneo;
            //pagtoPIXMT.vPag = 100;
            //pagtoPIXMT.tpIntegra = TpIntegra.tiPagIntegrado;
            //pagtoPIXMT.CNPJ = "99999999999999";
            //pagtoPIXMT.cAut = "123456789012345678901234657890123456789012346578901234567890123456789012345678901234567980";
            //pagtoPIXMT.CNPJReceb = "12345678901234";
            //pagtoPIXMT.idTermPag = "12345678901234567890";

            //notaFiscal.Pagamentos.Add(pagtoPIXMT);


            //-->Exemplo pagamento cartão débito/crédito para o Rio Grande do Sul.
            //PagamentoNFe pagtoCartaoRS = new PagamentoNFe();
            //pagtoCartaoRS.tPag = FormaPagamento.fpCartaoCredito;

            //pagtoCartaoRS.vPag = 100;
            //pagtoCartaoRS.tpIntegra = TpIntegra.tiPagIntegrado;
            //pagtoCartaoRS.cAut = "123456789012345678901234567890";

            //notaFiscal.Pagamentos.Add(pagtoCartaoRS);

            //-->Exemplo pagamento PIX para o Rio Grande do Sul.
            //PagamentoNFe pagtoPIXRS = new PagamentoNFe();
            //pagtoPIXRS.tPag = FormaPagamento.fpPagamentoInstantaneo;
            //pagtoPIXRS.vPag = 100;
            //pagtoPIXRS.tpIntegra = TpIntegra.tiPagIntegrado;
            ////pagtoPIXRS.CNPJ = "99999999999999";
            //pagtoPIXRS.cAut = "123456789012345678901234657890123456789012346578901234567890123456789012345678901234567980";
            ////pagtoPIXRS.CNPJReceb = "13245678901234";
            ////pagtoPIXRS.idTermPag = "12345678901234567890";

            //notaFiscal.Pagamentos.Add(pagtoPIXRS);


            return notaFiscal.ToString();
        }

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                ACBrNFe.Config.AtualizarXMLCancelado = ckbAtualizarXML.Checked;
                ACBrNFe.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBrNFe.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBrNFe.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                ACBrNFe.Config.ModeloDF = cmbModeloDocumento.GetSelectedValue<ModeloNFe>();
                ACBrNFe.Config.VersaoDF = cmbVersaoDF.GetSelectedValue<VersaoNFe>();
                ACBrNFe.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                ACBrNFe.Config.SalvarWS = ckbSalvar.Checked;
                ACBrNFe.Config.PathSalvar = txtLogs.Text;
                ACBrNFe.Config.PathSchemas = txtSchemaPath.Text;
                ACBrNFe.Config.IdCSC = txtIdCSC.Text;
                ACBrNFe.Config.CSC = txtCSC.Text;

                //Config Webservice
                ACBrNFe.Config.DFe.UF = cmbUfDestino.Text;

                ACBrNFe.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBrNFe.Config.Timeout = (int)nudTimeOut.Value;
                ACBrNFe.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBrNFe.Config.Visualizar = ckbVisualizar.Checked;
                ACBrNFe.Config.SalvarWS = ckbSalvarSOAP.Checked;
                ACBrNFe.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
                ACBrNFe.Config.AguardarConsultaRet = (int)nudAguardar.Value;
                ACBrNFe.Config.Tentativas = (int)nudTentativas.Value;
                ACBrNFe.Config.IntervaloTentativas = (int)nudIntervalos.Value;

                ACBrNFe.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBrNFe.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrNFe.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBrNFe.Config.Proxy.Senha = txtProxySenha.Text;

                //Config Certificado
                ACBrNFe.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBrNFe.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBrNFe.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBrNFe.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBrNFe.Config.DFe.Senha = txtCertPassword.Text;
                ACBrNFe.Config.DFe.NumeroSerie = txtCertNumero.Text;
                ACBrNFe.Config.DFe.DadosPFX = txtDadosPFX.Text;

                //Config Arquivos
                ACBrNFe.Config.SalvarArq = ckbSalvarArqs.Checked;
                ACBrNFe.Config.SepararPorMes = ckbPastaMensal.Checked;
                ACBrNFe.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                ACBrNFe.Config.EmissaoPathNFe = ckbEmissaoPathNFe.Checked;
                ACBrNFe.Config.SalvarEvento = ckbSalvaPathEvento.Checked;
                ACBrNFe.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
                ACBrNFe.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
                ACBrNFe.Config.PathNFe = txtArqNFe.Text;
                ACBrNFe.Config.PathInu = txtArqInu.Text;
                ACBrNFe.Config.PathEvento = txtArqEvento.Text;

                //Config Documento Auxiliar
                ACBrNFe.Config.DANFe.PathLogo = txtLogomarca.Text;
                ACBrNFe.Config.DANFe.TipoDANFE = rdbRetrato.Checked ? TipoDANFE.tiRetrato : TipoDANFE.tiPaisagem;

                var relNFCe = rdbFortes.Checked ? TipoRelatorioBobina.tpFortes :
                    rdbEscPos.Checked ? TipoRelatorioBobina.tpEscPos : TipoRelatorioBobina.tpFortesA4;
                ACBrNFe.Config.DANFe.NFCe.TipoRelatorioBobina = relNFCe;

                ACBrNFe.Config.PosPrinter.Modelo = cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>();
                ACBrNFe.Config.PosPrinter.Porta = cbbPortas.Text;
                ACBrNFe.Config.PosPrinter.ColunasFonteNormal = (int)nudColunas.Value;
                ACBrNFe.Config.PosPrinter.EspacoEntreLinhas = (int)nudEspacos.Value;
                ACBrNFe.Config.PosPrinter.LinhasBuffer = (int)nudBuffer.Value;
                ACBrNFe.Config.PosPrinter.LinhasEntreCupons = (int)nudLinhasPular.Value;
                ACBrNFe.Config.PosPrinter.ControlePorta = cbxControlePorta.Checked;
                ACBrNFe.Config.PosPrinter.CortaPapel = cbxCortarPapel.Checked;
                ACBrNFe.Config.PosPrinter.TraduzirTags = cbxTraduzirTags.Checked;
                ACBrNFe.Config.PosPrinter.IgnorarTags = cbxIgnorarTags.Checked;
                ACBrNFe.Config.PosPrinter.PaginaDeCodigo = cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>();

                //Config Email
                ACBrNFe.Config.Email.Nome = txtNome.Text;
                ACBrNFe.Config.Email.Conta = txtEmail.Text;
                ACBrNFe.Config.Email.Usuario = txtUsuario.Text;
                ACBrNFe.Config.Email.Senha = txtSenha.Text;
                ACBrNFe.Config.Email.Servidor = txtHost.Text;
                ACBrNFe.Config.Email.Porta = nudPorta.Text;
                ACBrNFe.Config.Email.SSL = ckbSSL.Checked;
                ACBrNFe.Config.Email.TLS = ckbTLS.Checked;

                ACBrNFe.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            ACBrNFe.ConfigLer(file);

            //Config Geral
            ckbAtualizarXML.Checked = ACBrNFe.Config.AtualizarXMLCancelado;
            ckbExibirErroSchema.Checked = ACBrNFe.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBrNFe.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(ACBrNFe.Config.FormaEmissao);
            cmbModeloDocumento.SetSelectedValue(ACBrNFe.Config.ModeloDF);
            cmbVersaoDF.SetSelectedValue(ACBrNFe.Config.VersaoDF);
            ckbRetirarAcentos.Checked = ACBrNFe.Config.RetirarAcentos;
            ckbSalvar.Checked = ACBrNFe.Config.SalvarWS;
            txtLogs.Text = ACBrNFe.Config.PathSalvar;
            txtSchemaPath.Text = ACBrNFe.Config.PathSchemas;
            txtIdCSC.Text = ACBrNFe.Config.IdCSC;
            txtCSC.Text = ACBrNFe.Config.CSC;

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrNFe.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBrNFe.Config.SSLType);
            nudTimeOut.Value = ACBrNFe.Config.Timeout;

            var ambiente = ACBrNFe.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrNFe.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBrNFe.Config.SalvarWS;
            ckbAjustarAut.Checked = ACBrNFe.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = ACBrNFe.Config.AguardarConsultaRet;
            nudTentativas.Value = ACBrNFe.Config.Tentativas;
            nudIntervalos.Value = ACBrNFe.Config.IntervaloTentativas;
            txtProxyServidor.Text = ACBrNFe.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrNFe.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrNFe.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrNFe.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrNFe.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrNFe.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrNFe.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrNFe.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrNFe.Config.DFe.Senha;
            txtCertNumero.Text = ACBrNFe.Config.DFe.NumeroSerie;
            txtDadosPFX.Text = ACBrNFe.Config.DFe.DadosPFX;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrNFe.Config.SalvarArq;
            ckbPastaMensal.Checked = ACBrNFe.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = ACBrNFe.Config.AdicionarLiteral;
            ckbEmissaoPathNFe.Checked = ACBrNFe.Config.EmissaoPathNFe;
            ckbSalvaPathEvento.Checked = ACBrNFe.Config.SalvarEvento;
            ckbSepararPorCNPJ.Checked = ACBrNFe.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = ACBrNFe.Config.SepararPorModelo;
            txtArqNFe.Text = ACBrNFe.Config.PathNFe;
            txtArqInu.Text = ACBrNFe.Config.PathInu;
            txtArqEvento.Text = ACBrNFe.Config.PathEvento;

            //Config Documento Auxiliar
            txtLogomarca.Text = ACBrNFe.Config.DANFe.PathLogo;
            var tipoImpressao = ACBrNFe.Config.DANFe.TipoDANFE;
            rdbRetrato.Checked = tipoImpressao == TipoDANFE.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoDANFE.tiPaisagem;

            var relNFCe = ACBrNFe.Config.DANFe.NFCe.TipoRelatorioBobina;
            rdbFortes.Checked = relNFCe == TipoRelatorioBobina.tpFortes;
            rdbEscPos.Checked = relNFCe == TipoRelatorioBobina.tpEscPos;
            rdbFortesA4.Checked = relNFCe == TipoRelatorioBobina.tpFortesA4;

            cbbModelo.SetSelectedValue(ACBrNFe.Config.PosPrinter.Modelo);
            cbbPortas.SelectedItem = ACBrNFe.Config.PosPrinter.Porta;
            nudColunas.Value = ACBrNFe.Config.PosPrinter.ColunasFonteNormal;
            nudEspacos.Value = ACBrNFe.Config.PosPrinter.EspacoEntreLinhas;
            nudBuffer.Value = ACBrNFe.Config.PosPrinter.LinhasBuffer;
            nudLinhasPular.Value = ACBrNFe.Config.PosPrinter.LinhasEntreCupons;
            cbxControlePorta.Checked = ACBrNFe.Config.PosPrinter.ControlePorta;
            cbxCortarPapel.Checked = ACBrNFe.Config.PosPrinter.CortaPapel;
            cbxTraduzirTags.Checked = ACBrNFe.Config.PosPrinter.TraduzirTags;
            cbxIgnorarTags.Checked = ACBrNFe.Config.PosPrinter.IgnorarTags;
            cbbPaginaCodigo.SetSelectedValue(ACBrNFe.Config.PosPrinter.PaginaDeCodigo);

            //Config Email
            txtNome.Text = ACBrNFe.Config.Email.Nome;
            txtEmail.Text = ACBrNFe.Config.Email.Conta;
            txtUsuario.Text = ACBrNFe.Config.Email.Usuario;
            txtSenha.Text = ACBrNFe.Config.Email.Senha;
            txtHost.Text = ACBrNFe.Config.Email.Servidor;
            nudPorta.Text = ACBrNFe.Config.Email.Porta;
            ckbSSL.Checked = ACBrNFe.Config.Email.SSL;
            ckbTLS.Checked = ACBrNFe.Config.Email.TLS;
        }

        private void CheckNFeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibNFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrNFe.LimparLista();

            if (xml)
                CarregarNFeXml();
            else
                CarregarNFeIni();
        }

        private void CarregarNFeIni()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrNFe.CarregarINI(arquivoIni);
        }

        private void CarregarNFeXml()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrNFe.CarregarXML(arquivoIni);
        }

        #endregion Methods

        #region EventHandlers

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void BtnSalvar_Click(object sender, EventArgs e)
        {
            try
            {
                SalvarConfig();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void BtnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void BtnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            var ret = ACBrNFe.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }
        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = ACBrNFe.OpenSSLInfo();
            rtbRespostas.AppendText(ret);
        }

        private void btnArqNFe_Click(object sender, EventArgs e)
        {
            txtArqNFe.Text = Helpers.SelectFolder();
        }

        private void btnArqEvento_Click(object sender, EventArgs e)
        {
            txtArqEvento.Text = Helpers.SelectFolder();
        }

        private void btnArqInu_Click(object sender, EventArgs e)
        {
            txtArqInu.Text = Helpers.SelectFolder();
        }

        private void btnLogomarca_Click(object sender, EventArgs e)
        {
            txtLogomarca.Text = Helpers.OpenFile("Image files (*.bmp, *.jpeg, *.png) | *.bmp; *.jpeg; *.png");
        }

        private void btnStatusServ_Click(object sender, EventArgs e)
        {
            try
            {
                var retorno = ACBrNFe.StatusServico();
                rtbRespostas.AppendText(retorno.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarXml_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFe.LimparLista();
                CarregarNFeIni();

                ACBrNFe.Assinar();
                var ret = ACBrNFe.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarSincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFe.Enviar(aLote, sincrono: true);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarAssincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFe.Enviar(aLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarIni_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarXml_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista(true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparLista_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibNFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrNFe.LimparLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimir_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.LimparLista();
                ACBrNFe.CarregarXML(arquivoXml);
                ACBrNFe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.LimparLista();
                ACBrNFe.CarregarXML(arquivoXml);
                ACBrNFe.ImprimirPDF();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrNFe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnAssinar_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista(true);

                ACBrNFe.Assinar();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnValidarRegra_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista(true);

                rtbRespostas.AppendText(ACBrNFe.ValidarRegrasdeNegocios());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarChave_Click(object sender, EventArgs e)
        {
            try
            {
                var uf = 35;
                if (InputBox.Show("Gerar Chave", "Digite o codigo da UF", ref uf) != DialogResult.OK) return;

                var cod = 45812;
                if (InputBox.Show("Gerar Chave", "Digite o codigo da Númerico", ref cod) != DialogResult.OK) return;

                var doc = 55;
                if (InputBox.Show("Gerar Chave", "Digite o modelo do documento", ref doc) != DialogResult.OK) return;

                var serie = 1;
                if (InputBox.Show("Gerar Chave", "Digite a serie do documento", ref serie) != DialogResult.OK) return;

                var numero = 1;
                if (InputBox.Show("Gerar Chave", "Digite o numero do documento", ref numero) != DialogResult.OK) return;

                var emissao = 1;
                if (InputBox.Show("Gerar Chave", "Digite o tipo de emissão do documento", ref emissao) != DialogResult.OK) return;

                var cnpjCPF = "";
                if (InputBox.Show("Gerar Chave", "Digite o CPF/CNPJ para Gerar a Chave", ref cnpjCPF) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(cnpjCPF)) return;

                rtbRespostas.AppendText(ACBrNFe.GerarChave(uf, cod, doc, serie, numero, emissao, DateTime.Now, cnpjCPF));
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaXml_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrNFe.LimparLista();

                var ret = ACBrNFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaChave_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da NF-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrNFe.LimparLista();
                var ret = ACBrNFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recibo", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrNFe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCadastro_Click(object sender, EventArgs e)
        {
            try
            {
                var uf = "SP";
                var documento = "";
                var ie = false;

                if (InputBox.Show("WebServices Consultar: Cadastro", "UF", ref uf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Consultar: Cadastro", "Documento", ref documento) != DialogResult.OK) return;
                ie = MessageBox.Show(@"O documento é uma inscrição estadual ?", @"WebServices Consultar: Cadastro", MessageBoxButtons.YesNo) == DialogResult.Yes;

                var ret = ACBrNFe.ConsultaCadastro(uf, documento, ie);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                var aJustificativa = "";
                var eChave = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da NF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrNFe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                if (InputBox.Show("WebServices Eventos: Enviar", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;

                var ret = ACBrNFe.EnviarEvento(idLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrNFe.CarregarEventoINI(arquivoIni);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparListaEvento_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista de eventos ?", @"ACBrLibNFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrNFe.LimparListaEventos();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirEvento(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEventoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirEventoPDF(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmailEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrNFe.EnviarEmailEvento(destinatario, arquivoXmlEvento, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnInutilizar_Click(object sender, EventArgs e)
        {
            try
            {
                var ano = 1;
                var modelo = 57;
                var serie = 1;
                var numeroInicial = 1;
                var numeroFinal = 1;
                var aJustificativa = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices: Inutilização", "Ano", ref ano) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Modelo", ref modelo) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Serie", ref serie) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Número Inicial", ref numeroInicial) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Número Final", ref numeroFinal) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "CNPJ ou o CPF do emitente", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Justificativa", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrNFe.Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacao_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirInutilizacao(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacaoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirInutilizacaoPDF(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorChave_Click(object sender, EventArgs e)
        {
            try
            {
                var codUf = 35;
                var cnpj = "";
                var chave = "";

                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Chave da NFe", ref chave) != DialogResult.OK) return;

                var ret = ACBrNFe.DistribuicaoDFePorChave(codUf, cnpj, chave);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorNSU_Click(object sender, EventArgs e)
        {
            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrNFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret.Resposta);
        }

        private void btnDFePorUltNSU_Click(object sender, EventArgs e)
        {
            var codUf = 35;
            var cnpj = "";
            var eNsu = "";
            var ArquivoOuXml = Helpers.OpenFile("Arquivos Distribuição DFe (*-dist-dfe.xml)|*-dist-dfe.xml|Todos os Arquivos (*.*)|*.*",
                                                "Selecione um Arquivo de Distribuição para simular uma consulta ou feche para consultar o WebService");

            if (string.IsNullOrEmpty(ArquivoOuXml))
            {
                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;
            }

            var ret = ACBrNFe.DistribuicaoDFe(codUf, cnpj, eNsu, ArquivoOuXml);
            rtbRespostas.AppendText(ret.Resposta);
        }

        #endregion EventHandlers

        private async void btnSalvarPDF_ClickAsync(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.LimparLista();
                ACBrNFe.CarregarXML(arquivoXml);

                var nomeArquivo = Helpers.SaveFile("Salvar em PDF (*.pdf)|*.pdf|Todos os Arquivos (*.*)|*.*");
                
                using (FileStream aStream = File.Create(nomeArquivo))
                {
                    ACBrNFe.ImprimirPDF(aStream);
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
            var nfe = AlimentarDados();

            ACBrNFe.LimparLista();
            ACBrNFe.CarregarINI(nfe);

            try
            {
                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFe.Enviar(aLote);

                rtbRespostas.AppendLine("[ENVIO]");
                rtbRespostas.AppendLine("cUF=" + Convert.ToString(ret.Envio.CUF));
                rtbRespostas.AppendLine("tpAmb=" + ret.Envio.tpAmb.GetEnumValueOrInt());
                rtbRespostas.AppendLine("cStat=" + Convert.ToString(ret.Envio.CStat));
                rtbRespostas.AppendLine("cMsg=" + Convert.ToString(ret.Envio.Msg));
                rtbRespostas.AppendLine("xMotivo=" + ret.Envio.XMotivo);
                rtbRespostas.AppendLine("nRec=" + Convert.ToString(ret.Envio.NRec));
                rtbRespostas.AppendLine("Protocolo=" + ret.Envio.NProt);
                rtbRespostas.AppendLine("Versao=" + ret.Envio.Versao);
                rtbRespostas.AppendLine("DhRec=" + Convert.ToString(ret.Envio.DhRecbto));

                rtbRespostas.AppendLine("[RETORNO]");
                rtbRespostas.AppendLine("cUF=" + Convert.ToString(ret.Retorno.CUF));
                rtbRespostas.AppendLine("tpAmb=" + ret.Retorno.tpAmb.GetEnumValueOrInt());
                rtbRespostas.AppendLine("cStat=" + Convert.ToString(ret.Retorno.CStat));
                rtbRespostas.AppendLine("cMsg=" + Convert.ToString(ret.Retorno.cMsg));
                rtbRespostas.AppendLine("xMsg=" + ret.Retorno.xMsg);
                rtbRespostas.AppendLine("xMotivo=" + ret.Retorno.XMotivo);
                rtbRespostas.AppendLine("nRec=" + Convert.ToString(ret.Retorno.nRec));
                rtbRespostas.AppendLine("Protocolo=" + ret.Retorno.Protocolo);
                rtbRespostas.AppendLine("ChaveMDFe=" + ret.Retorno.ChaveDFe);
                rtbRespostas.AppendLine("DhRec=" + Convert.ToString(ret.Retorno.DhRecbto));

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }
    }
}