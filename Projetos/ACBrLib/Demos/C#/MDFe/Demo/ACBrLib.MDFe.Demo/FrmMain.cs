using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.MDFe;
using ACBrLib.Core.Extensions;

namespace ACBrLib.MDFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrMDFe ACBrMDFe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            ACBrMDFe = new ACBrMDFe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            ACBrMDFe.Dispose();
            ACBrMDFe = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();

            try
            {
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbVersaoDF.EnumDataSource(VersaoMDFe.ve300);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);

                // Altera as config de log
                ACBrMDFe.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrMDFe.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
                ACBrMDFe.ConfigGravar();

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        #endregion Constructors

        #region Methods

        private String AlimendarDados()
        {
            /*
             * Exemplo de uso da classe Manifesto para geração dos .ini de entrada da ACBrlib  
             * Preenchimento com os dados mínimos para emissão de MDFe
             * Descomente as demais classes que precisar usar
            */

            var manifesto = new Manifesto();

            //InfMDFe
            manifesto.InfMDFe.Versao = VersaoMDFe.ve300;

            //Identificação (IDEMDFe)
            manifesto.Identificacao.tpEmit = TpEmitenteMDFe.teTransportadora;
            manifesto.Identificacao.mod = "58";
            manifesto.Identificacao.serie = 1;
            manifesto.Identificacao.nMDF = 1;
            manifesto.Identificacao.modal = ModalMDFe.moRodoviario;
            manifesto.Identificacao.dhEmi = DateTime.Now;
            manifesto.Identificacao.tpEmis = TipoEmissao.teNormal;
            manifesto.Identificacao.procEmi = ProcessoEmissao.peAplicativoContribuinte;
            manifesto.Identificacao.verProc = "1.0";
            manifesto.Identificacao.UFIni = "SP";
            manifesto.Identificacao.UFFim = "SE";
            manifesto.Identificacao.tpTransp = TransportadorMDFe.ttNenhum;
            manifesto.Identificacao.dhIniViagem = DateTime.Now;
            manifesto.Identificacao.indCanalVerde = false;
            manifesto.Identificacao.indCarregaPosterior = false;
            manifesto.Identificacao.cUF = 35;

            #region Emitente 
            //Emitente
            manifesto.Emitente.CNPJCPF = "99999999999999";
            manifesto.Emitente.IE = "99999999999";
            manifesto.Emitente.xNome = "Transportes ACBr SA";
            manifesto.Emitente.xFant = "Transportes ACBr";
            manifesto.Emitente.xLgr = "Rua: Cel Aureliano de Camargo";
            manifesto.Emitente.nro = "963";
            manifesto.Emitente.xCpl = "casa";
            manifesto.Emitente.xBairro = "Centro";
            manifesto.Emitente.cMun = 3554003;
            manifesto.Emitente.xMun = "Tatui";
            manifesto.Emitente.CEP = 18280000;
            manifesto.Emitente.UF = "SP";
            manifesto.Emitente.fone = "(15) 2105-0750";
            manifesto.Emitente.email = "forum@projetoacbr.com.br";
            #endregion Emitente

            #region Informações Carregamento 
            //InfMunCarregaMDFe
            var munCarrega = new InfMunCarregaMDFe();
            munCarrega.cMunCarrega = 3554003;
            munCarrega.xMunCarrega = "Tatui";
            manifesto.InfMunCarrega.Add(munCarrega);
            #endregion Informações Carregamento 

            #region Informações Percurso
            //InfPercurso
            var percUF1 = new InfPercurso();
            percUF1.UFPer = "MG";
            manifesto.InfPercurso.Add(percUF1);

            var percUF2 = new InfPercurso();
            percUF2.UFPer = "BA";
            manifesto.InfPercurso.Add(percUF2);
            #endregion MunicipioDescarga

            #region Modal Rodoviário


            var modal = new ModalRodoMDFe();
            modal.codAgPorto = "";

            #region InfANTT
            /*var disp1 = new DispMDFe();
            disp1.CNPJForn = "05481336000137";
            disp1.CNPJPg = "05481336000137";
            disp1.nCompra = "50";
            disp1.vValePed = 20;
            disp1.tpValePed = TpValePed.tvpNenhum;*/

            var ciot = new InfCIOTMDFe();
            ciot.CNPJCPF = "99999999999999";
            ciot.CIOT = "121212121212";

            var contratante = new InfContratanteMDFe();
            contratante.CNPJCPF = "99999999999999";
            contratante.xNome = "nome contratante";
            contratante.idEstrangeiro = "";

            #region Informações Pagamento
            /*var comp1 = new CompMDFe();
            comp1.tpComp = TpComp.tcOutros;
            comp1.xComp = "Descricao";
            comp1.vComp = 0;

            var prazo1 = new InfPrazoMDFe();
            prazo1.nParcela = 1;
            prazo1.vParcela = 0;
            prazo1.dVenc = DateTime.Now;

            var pag1 = new InfPagMDFe();
            pag1.xNome = "Nome pagador";
            pag1.CNPJCPF = "99999999999999";
            pag1.idEstrangeiro = "";
            pag1.vContrato = 0;
            pag1.indAltoDesemp = false;
            pag1.indPag = IndPag.ipVista;
            pag1.vAdiant = 0;
            pag1.InfBanco.codBanco = "";
            pag1.InfBanco.codAgencia = "";
            pag1.InfBanco.CNPJIPEF = "";
            pag1.InfBanco.PIX = "";
            pag1.Comp.Add(comp1);    
            pag1.InfPrazo.Add(prazo1);*/

            #endregion Informações Pagamento

            modal.InfANTT.RNTRC = "22222222";
            modal.InfANTT.valePed.CategCombVeic = CategCombVeic.tcVeicCom2Eixos;
            modal.InfANTT.InfCIOT.Add(ciot);
            //modal.InfANTT.valePed.disp.Add(disp1);
            modal.InfANTT.InfContratante.Add(contratante);
            //modal.InfANTT.InfPag.Add(pag1);

            #endregion InfANTT

            #region VeicTracao   

            var condutor = new CondutorMDFe();
            condutor.CPF = "99999999999";
            condutor.xNome = "Nome condutor";

            modal.VeicTracao.cInt = "1";
            modal.VeicTracao.placa = "DIJ2121";
            modal.VeicTracao.RENAVAM = "";
            modal.VeicTracao.tara = 0;
            modal.VeicTracao.capKG = 0;
            modal.VeicTracao.capM3 = 0;
            modal.VeicTracao.tpRod = TipoRodado.trTruck;
            modal.VeicTracao.tpCar = TipoCarroceria.tcFechada;
            modal.VeicTracao.UF = "SP";
            /*modal.VeicTracao.Proprietario.CNPJCPF = "99999999999999";
            modal.VeicTracao.Proprietario.RNTRC = "99999999";
            modal.VeicTracao.Proprietario.xNome = "Nome proprietario";
            modal.VeicTracao.Proprietario.IE = "ISENTO";
            modal.VeicTracao.Proprietario.UFProp = "SP";
            modal.VeicTracao.Proprietario.tpProp = TpProp.tpOutros;*/
            modal.VeicTracao.Condutor.Add(condutor);

            #endregion VeicTracao

            #region Reboque            

            var reboque1 = new ReboqueMDFe();
            reboque1.cInt = "1";
            reboque1.placa = "DIJ2122";
            reboque1.RENAVAM = "";
            reboque1.tara = 0;
            reboque1.capKG = 0;
            reboque1.capM3 = 0;
            reboque1.tpCar = TipoCarroceria.tcFechada;
            reboque1.UF = "SP";
            /* reboque1.Proprietario.CNPJCPF = "999999999";
             reboque1.Proprietario.RNTRC = "99999999";
             reboque1.Proprietario.xNome = "Nome Proprietário";
             reboque1.Proprietario.IE = "Isento";
             reboque1.Proprietario.UFProp = "SP";
             reboque1.Proprietario.tpProp = TpProp.tpOutros;*/

            modal.Reboque.Add(reboque1);

            #endregion Reboque

            #region Lacre            

            var lacreModal1 = new LacreMDFe();
            lacreModal1.nLacre = "0000001";

            modal.Lacres.Add(lacreModal1);

            #endregion Lacre

            //ModalRodoviario
            manifesto.Modal = modal;

            #endregion Modal Rodoviário

            #region DadosCTe
            //InfCTeMDFe - para vincular CTes ao MDFe

            var periMDFe = new PeriMDFe();
            periMDFe.nONU = "4545";
            periMDFe.xNomeAE = "Nome AE";
            periMDFe.xClaRisco = "Classe Risco";
            periMDFe.qTotProd = "1";
            periMDFe.qVolTipo = "1";
            periMDFe.grEmb = "";

            //InfEntregaParcial
            //var infEntregaParcial = new InfEntregaParcialMDFe();
            //infEntregaParcial.qtdTotal = 1;
            //infEntregaParcial.qtdParcial = 2;

            //LacreMDFe
            var lacre = new LacreMDFe();
            lacre.nLacre = "000001";

            //InfUnidCargaMDFe
            /*var infUnidadeCargaMDFe = new InfUnidCargaMDFe();
            infUnidadeCargaMDFe.tpUnidCarga = UnidCarga.ucPallet;
            infUnidadeCargaMDFe.idUnidCarga = "1";
            infUnidadeCargaMDFe.qtdRat = 0;
            infUnidadeCargaMDFe.lacUnidCarga.Add(lacre);*/

            //LacreMDFe
            var lacreUnid = new LacreMDFe();
            lacreUnid.nLacre = "000001";

            //InfUnidTransp
            /*var infUnidTranspMDFe = new InfUnidTranspMDFe();
            infUnidTranspMDFe.idUnidTransp = "";
            infUnidTranspMDFe.tpUnidTransp = UnidTransp.utRodoTracao;
            infUnidTranspMDFe.qtdRat = 0;
            infUnidTranspMDFe.lacUnidTransp.Add(lacreUnid);
            infUnidTranspMDFe.infUnidCarga.Add(infUnidadeCargaMDFe);*/

            var infCTe = new InfCTeMDFe();
            infCTe.chCTe = "99999999999999999999999999999999999999999999";
            infCTe.SegCodBarra = "";
            infCTe.indReentrega = "";
            infCTe.Peri.Add(periMDFe);
            //infCTe.InfUnidTransp.Add(infUnidTranspMDFe);
            //infCTe.InfEntregaParcial.Add(infEntregaParcial);
            #endregion DadosCTe

            #region DadosNFe
            //InfNFeMDFe - para vincular NFes ao MDFe
            /*
            var periNFe = new PeriMDFe();
            periNFe.nONU = "0000";
            periNFe.xNomeAE = "Nome AE";
            periNFe.xClaRisco = "Classe Risco";
            periNFe.qTotProd = "1";
            periNFe.qVolTipo = "2";
            periNFe.grEmb = "";

            //InfEntregaParcial
            var infEntregaParcialNFe = new InfEntregaParcialMDFe();
            infEntregaParcialNFe.qtdTotal = 1;
            infEntregaParcialNFe.qtdParcial = 2; 

            //LacreMDFe
            var lacreNFe = new LacreMDFe();
            lacreNFe.nLacre = "0000000";

            //InfUnidCargaMDFe
            var infUnidadeCargaMDFeNF = new InfUnidCargaMDFe();
            infUnidadeCargaMDFeNF.tpUnidCarga = UnidCarga.ucPallet;
            infUnidadeCargaMDFeNF.idUnidCarga = "1";
            infUnidadeCargaMDFeNF.qtdRat = 0;
            infUnidadeCargaMDFeNF.lacUnidCarga.Add(lacreNFe);

            //LacreMDFe
            var lacreUnidNF = new LacreMDFe();
            lacreUnidNF.nLacre = "0000000";

            //InfUnidTransp
            var infUnidTranspMDFeNF = new InfUnidTranspMDFe();
            infUnidTranspMDFeNF.idUnidTransp = "";
            infUnidTranspMDFeNF.tpUnidTransp = UnidTransp.utRodoTracao;
            infUnidTranspMDFeNF.qtdRat = 0;
            infUnidTranspMDFeNF.lacUnidTransp.Add(lacreUnidNF);
            infUnidTranspMDFeNF.infUnidCarga.Add(infUnidadeCargaMDFeNF);

            var infNFe = new InfNFeMDFe();
            infNFe.chNFe = "12345678901234567890123456789012345678901234";
            infNFe.SegCodBarra = "";
            infNFe.indReentrega = "";
            infNFe.Peri.Add(periNFe);
            infNFe.InfUnidTransp.Add(infUnidTranspMDFeNF);
            infNFe.InfEntregaParcial.Add(infEntregaParcialNFe); */
            #endregion DadosNFe

            #region Dados MDFe
            //InfMDFe - para vincular MDFes

            /*var periManif = new PeriMDFe();
            periManif.nONU = "0000";
            periManif.xNomeAE = "Nome AE";
            periManif.xClaRisco = "Classe Risco";
            periManif.qTotProd = "1";
            periManif.qVolTipo = "2";
            periManif.grEmb = "";

            //InfEntregaParcial
            var infEntregaParcialManif = new InfEntregaParcialMDFe();
            infEntregaParcialManif.qtdTotal = 1;
            infEntregaParcialManif.qtdParcial = 2; 

            //LacreMDFe
            var lacreManif = new LacreMDFe();
            lacreManif.nLacre = "0000000";

            //InfUnidCargaMDFe
            var infUnidadeCargaManif = new InfUnidCargaMDFe();
            infUnidadeCargaManif.tpUnidCarga = UnidCarga.ucPallet;
            infUnidadeCargaManif.idUnidCarga = "1";
            infUnidadeCargaManif.qtdRat = 0;
            infUnidadeCargaManif.lacUnidCarga.Add(lacreManif);

            //LacreMDFe
            var lacreUnidManif = new LacreMDFe();
            lacreUnidManif.nLacre = "0000000";

            //InfUnidTransp
            var infUnidTranspManif = new InfUnidTranspMDFe();
            infUnidTranspManif.idUnidTransp = "";
            infUnidTranspManif.tpUnidTransp = UnidTransp.utRodoTracao;
            infUnidTranspManif.qtdRat = 0;
            infUnidTranspManif.lacUnidTransp.Add(lacreUnidManif);
            infUnidTranspManif.infUnidCarga.Add(infUnidadeCargaManif);

            var infMDFe = new InfMDFeTransp();
            infMDFe.chMDFe = "12345678901234567890123456789012345678901234";  
            infMDFe.indReentrega = "";
            infMDFe.Peri.Add(periManif);
            infMDFe.InfUnidTransp.Add(infUnidTranspManif);
            infMDFe.InfEntregaParcial.Add(infEntregaParcialManif); */
            #endregion DadosMDFe

            #region Informações Descarga
            //InfMunDescargaMDFe

            var munDescarga = new InfMunDescargaMDFe();
            munDescarga.cMunDescarga = 2800308;
            munDescarga.xMunDescarga = "Aracaju";
            munDescarga.InfCTe.Add(infCTe);                             //Utilizado se for vincular CTes
            //munDescarga.InfNFe.Add(infNFe);                           //Utilizado se for vincular NFes
            //munDescarga.InfMDFeTransp.Add(infMDFe);                   //Utilizado se for vincular MDFes

            manifesto.InfDoc.infMunDescarga.Add(munDescarga);
            #endregion MunicipioDescarga

            #region Informações Seguro
            //AverbacaoMDFe

            var averb = new AverbacaoMDFe();
            averb.nAver = "534534";

            var seg = new SeguroMDFe();
            seg.respSeg = RspSegMDFe.rsEmitente;
            seg.CNPJ = "99999999999999";
            seg.xSeg = "Seguradora ABC";
            seg.CNPJ = "99999999999999";
            seg.nApol = "534534";
            seg.Averb.Add(averb);

            manifesto.Seg.Add(seg);

            //manifesto.Seg
            #endregion Informações Seguro

            #region Produto Predominante
            //AverbacaoMDFe

            manifesto.ProdPred.tpCarga = TpCarga.tcCargaGeral;
            manifesto.ProdPred.xProd = "Descricao Produto";
            manifesto.ProdPred.cEAN = "SEM GTIN";
            manifesto.ProdPred.NCM = "84719012";
            manifesto.ProdPred.InfLocalCarrega.CEP = 18460000;
            manifesto.ProdPred.InfLocalCarrega.latitude = 0;
            manifesto.ProdPred.InfLocalCarrega.longitude = 0;
            manifesto.ProdPred.InfLocalDescarrega.CEP = 18280000;
            manifesto.ProdPred.InfLocalDescarrega.latitude = 0;
            manifesto.ProdPred.InfLocalDescarrega.longitude = 0;

            #endregion Produto Predominante

            #region Total MDFe
            //TotalMDFe

            manifesto.Tot.qCTe = 1;
            manifesto.Tot.qMDFe = 0;
            manifesto.Tot.qNFe = 0;
            manifesto.Tot.vCarga = 1.11M;
            manifesto.Tot.cUnid = UnidMed.uKG;
            manifesto.Tot.qCarga = 1;

            #endregion Total MDFe

            #region Lacre MDFe
            //LacreMDFe
            var lacreMDFe = new LacreMDFe();
            lacreMDFe.nLacre = "000001";

            manifesto.Lacres.Add(lacreMDFe);

            #endregion Lacre MDFe

            #region AutXML
            //AutXML

            /*var autXML = new AutXML();
            autXML.CNPJCPF = "99999999999999";

            manifesto.AutXml.Add(autXML);*/

            #endregion AutXML;

            #region Dados Adicionais
            //DadosAdicionais

            /*manifesto.DadosAdicionais.infAdFisco = "";
            manifesto.DadosAdicionais.infCpl = "";*/

            #endregion Dados Adicionais;

            #region Informações Responsável Tecnico
            //InfRespTec

            /*manifesto.InfRespTec.CNPJ = "99999999999999";
            manifesto.InfRespTec.xContato = "XXX Software";
            manifesto.InfRespTec.fone = "9999999999";
            manifesto.InfRespTec.email = "xxx@yyy.com";*/

            #endregion Informações Responsável Tecnico;

            return manifesto.ToString();

        }

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                ACBrMDFe.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBrMDFe.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBrMDFe.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                ACBrMDFe.Config.VersaoDF = cmbVersaoDF.GetSelectedValue<VersaoMDFe>();
                ACBrMDFe.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                ACBrMDFe.Config.SalvarWS = ckbSalvar.Checked;
                ACBrMDFe.Config.PathSalvar = txtLogs.Text;
                ACBrMDFe.Config.PathSchemas = txtSchemaPath.Text;

                //Config Webservice
                ACBrMDFe.Config.DFe.UF = cmbUfDestino.Text;
                ACBrMDFe.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBrMDFe.Config.Timeout = (int)nudTimeOut.Value;
                ACBrMDFe.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBrMDFe.Config.Visualizar = ckbVisualizar.Checked;
                ACBrMDFe.Config.SalvarWS = ckbSalvarSOAP.Checked;
                ACBrMDFe.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
                ACBrMDFe.Config.AguardarConsultaRet = (int)nudAguardar.Value;
                ACBrMDFe.Config.Tentativas = (int)nudTentativas.Value;
                ACBrMDFe.Config.IntervaloTentativas = (int)nudIntervalos.Value;
                ACBrMDFe.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBrMDFe.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrMDFe.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBrMDFe.Config.Proxy.Senha = txtProxySenha.Text;

                //Config Certificado
                ACBrMDFe.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBrMDFe.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBrMDFe.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBrMDFe.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBrMDFe.Config.DFe.Senha = txtCertPassword.Text;
                ACBrMDFe.Config.DFe.NumeroSerie = txtCertNumero.Text;

                //Config Arquivos
                ACBrMDFe.Config.SalvarGer = ckbSalvarArqs.Checked;
                ACBrMDFe.Config.SepararPorMes = ckbPastaMensal.Checked;
                ACBrMDFe.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                ACBrMDFe.Config.EmissaoPathMDFe = ckbEmissaoPathNFe.Checked;
                ACBrMDFe.Config.SalvarArq = ckbSalvaPathEvento.Checked;
                ACBrMDFe.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
                ACBrMDFe.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
                ACBrMDFe.Config.PathMDFe = txtArqMDFe.Text;
                ACBrMDFe.Config.PathEvento = txtArqEvento.Text;

                //Config Documento Auxiliar
                ACBrMDFe.Config.DAMDFe.PathLogo = txtLogomarca.Text;
                ACBrMDFe.Config.DAMDFe.TipoDAMDFe = rdbRetrato.Checked ? TipoDAMDFe.tiRetrato : TipoDAMDFe.tiPaisagem;

                //Config Email
                ACBrMDFe.Config.Email.Nome = txtNome.Text;
                ACBrMDFe.Config.Email.Conta = txtEmail.Text;
                ACBrMDFe.Config.Email.Usuario = txtUsuario.Text;
                ACBrMDFe.Config.Email.Senha = txtSenha.Text;
                ACBrMDFe.Config.Email.Servidor = txtHost.Text;
                ACBrMDFe.Config.Email.Porta = nudPorta.Text;
                ACBrMDFe.Config.Email.SSL = ckbSSL.Checked;
                ACBrMDFe.Config.Email.TLS = ckbTLS.Checked;
                ACBrMDFe.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig(string file = "ACBrlib.ini")
        {
            ACBrMDFe.ConfigLer(file);

            //Config Geral
            ckbExibirErroSchema.Checked = ACBrMDFe.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBrMDFe.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(ACBrMDFe.Config.FormaEmissao);
            cmbVersaoDF.SetSelectedValue(ACBrMDFe.Config.VersaoDF);
            ckbRetirarAcentos.Checked = ACBrMDFe.Config.RetirarAcentos;
            ckbSalvar.Checked = ACBrMDFe.Config.SalvarWS;
            txtLogs.Text = ACBrMDFe.Config.PathSalvar;
            txtSchemaPath.Text = ACBrMDFe.Config.PathSchemas;

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrMDFe.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBrMDFe.Config.SSLType);
            nudTimeOut.Value = ACBrMDFe.Config.Timeout;

            var ambiente = ACBrMDFe.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrMDFe.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBrMDFe.Config.SalvarWS;
            ckbAjustarAut.Checked = ACBrMDFe.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = ACBrMDFe.Config.AguardarConsultaRet;
            nudTentativas.Value = ACBrMDFe.Config.Tentativas;
            nudIntervalos.Value = ACBrMDFe.Config.IntervaloTentativas;
            txtProxyServidor.Text = ACBrMDFe.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrMDFe.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrMDFe.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrMDFe.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrMDFe.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrMDFe.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrMDFe.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrMDFe.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrMDFe.Config.DFe.Senha;
            txtCertNumero.Text = ACBrMDFe.Config.DFe.NumeroSerie;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrMDFe.Config.SalvarGer;
            ckbPastaMensal.Checked = ACBrMDFe.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = ACBrMDFe.Config.AdicionarLiteral;
            ckbEmissaoPathNFe.Checked = ACBrMDFe.Config.EmissaoPathMDFe;
            ckbSalvaPathEvento.Checked = ACBrMDFe.Config.SalvarArq;
            ckbSepararPorCNPJ.Checked = ACBrMDFe.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = ACBrMDFe.Config.SepararPorModelo;
            txtArqMDFe.Text = ACBrMDFe.Config.PathMDFe;
            txtArqEvento.Text = ACBrMDFe.Config.PathEvento;

            //Config Documento Auxiliar
            txtLogomarca.Text = ACBrMDFe.Config.DAMDFe.PathLogo;
            var tipoImpressao = ACBrMDFe.Config.DAMDFe.TipoDAMDFe;
            rdbRetrato.Checked = tipoImpressao == TipoDAMDFe.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoDAMDFe.tiPaisagem;

            //Config Email
            txtNome.Text = ACBrMDFe.Config.Email.Nome;
            txtEmail.Text = ACBrMDFe.Config.Email.Conta;
            txtUsuario.Text = ACBrMDFe.Config.Email.Usuario;
            txtSenha.Text = ACBrMDFe.Config.Email.Senha;
            txtHost.Text = ACBrMDFe.Config.Email.Servidor;
            nudPorta.Text = ACBrMDFe.Config.Email.Porta;
            ckbSSL.Checked = ACBrMDFe.Config.Email.SSL;
            ckbTLS.Checked = ACBrMDFe.Config.Email.TLS;
        }

        private void CheckMDFeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibMDFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrMDFe.LimparLista();

            if (xml)
                CarregarMDFeXml();
            else
                CarregarMDFeIni();
        }

        public bool validacaoEmail()
        {
            if (txtHost.Text == "")
            {
                errorProvider.SetError(txtHost, "Informe Host SMTP");
                return false;
            }

            if (txtUsuario.Text == "")
            {
                errorProvider.SetError(txtUsuario, "Informe Usuário");
                return false;
            }
            if (txtSenha.Text == "")
            {
                errorProvider.SetError(txtSenha, "Informe Senha");
                return false;
            }
            if (txtNome.Text == "")
            {
                errorProvider.SetError(txtNome, "Informe Nome do Proprietario do e-mail");
                return false;
            }
            if (txtEmail.Text == "")
            {
                errorProvider.SetError(txtEmail, "Informe e-mail do Proprietario");
                return false;
            }
            if (nudPorta.Value == 0)
            {
                errorProvider.SetError(nudPorta, "Informe porta de conexão");
                return false;
            }
            if (ckbSSL.Checked == false && ckbTLS.Checked == false)
            {
                errorProvider.SetError(ckbSSL, "Informe o certificado SSL");
                errorProvider.SetError(ckbTLS, "Informe o certificado TLS");
                return false;
            }

            {
                return true;
            }
        }

        public bool validacao()
        {
            errorProvider.Clear();

            if (txtSchemaPath.Text == "")
            {
                errorProvider.SetError(txtSchemaPath, "Informe Path com Schema");
                return false;
            }

            if (txtCertNumero.Text == "" && txtCertPath.Text == "")
            {
                errorProvider.SetError(txtCertNumero, "Informe o número de série");
                errorProvider.SetError(txtCertPath, "Informe o certificado");
                return false;
            }

            if (txtCertPath.Text != "" && txtCertPassword.Text == "")
            {
                errorProvider.SetError(txtCertPassword, "Informe a senha");
                return false;
            }

            if (cmbCrypt.Text == "cryNone")
            {
                errorProvider.SetError(cmbCrypt, "Informe Criptografia");
                return false;
            }
            if (cmbHttp.Text == "httpNone")
            {
                errorProvider.SetError(cmbHttp, "Informe o tipo SSL");
                return false;
            }
            if (cmbXmlSign.Text == "xsNone")
            {
                errorProvider.SetError(cmbXmlSign, "Informe assinatura do XML");
                return false;
            }

            if (cmbSSlType.Text != "LT_all") return true;
            errorProvider.SetError(cmbSSlType, "Informe o tipo SSL");
            return false;
        }

        private void CarregarMDFeIni()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrMDFe.CarregarINI(arquivoIni);
        }

        private void CarregarMDFeXml()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrMDFe.CarregarXML(arquivoIni);
        }

        #endregion Methods

        #region EventHandlers

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

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(txtDadosPFX.Text))
            {
                var cert = new X509Certificate2(Convert.FromBase64String(txtDadosPFX.Text), txtCertPassword.Text);
                MessageBox.Show(cert.SerialNumber);
            }

            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            var ret = ACBrMDFe.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }

        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = ACBrMDFe.OpenSSLInfo();
            rtbRespostas.AppendText(ret);
        }

        private void btnArqMDFe_Click(object sender, EventArgs e)
        {
            txtArqMDFe.Text = Helpers.SelectFolder();
        }

        private void btnArqEvento_Click(object sender, EventArgs e)
        {
            txtArqEvento.Text = Helpers.SelectFolder();
        }

        private void btnLogomarca_Click(object sender, EventArgs e)
        {
            txtLogomarca.Text = Helpers.OpenFile("Image files (*.bmp, *.jpeg, *.png) | *.bmp; *.jpeg; *.png");
        }

        private void btnSalvar_Click(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void btnGerarXml_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                ACBrMDFe.LimparLista();
                CarregarMDFeIni();

                ACBrMDFe.Assinar();
                var ret = ACBrMDFe.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarSincrono_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote, sincrono: true);
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
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote);
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
                CheckMDFeLista();
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
                CheckMDFeLista(true);
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
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibMDFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrMDFe.LimparLista();
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.LimparLista();
                ACBrMDFe.CarregarXML(arquivoXml);
                ACBrMDFe.Imprimir(bMostrarPreview: true);
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
                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.LimparLista();
                ACBrMDFe.CarregarXML(arquivoXml);
                ACBrMDFe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            if (!validacaoEmail())
            {
                MessageBox.Show(@"Erro - Verifique as configurações de E-mail");
                return;
            }

            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrMDFe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
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
                CheckMDFeLista(true);

                ACBrMDFe.Assinar();
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
                CheckMDFeLista(true);

                rtbRespostas.AppendText(ACBrMDFe.ValidarRegrasdeNegocios());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnStatusServ_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                rtbRespostas.AppendText(ACBrMDFe.StatusServico().Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaXml_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrMDFe.LimparLista();

                var ret = ACBrMDFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaChave_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da MDF-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrMDFe.LimparLista();
                var ret = ACBrMDFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recibo", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrMDFe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsNaoEncerrados_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var aCNPJ = "";
                if (InputBox.Show("WebServices Consultar: Não Encerrados", "CNPJ.", ref aCNPJ) != DialogResult.OK) return;

                var ret = ACBrMDFe.ConsultaMDFeNaoEnc(aCNPJ);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var idLote = 1;
                var aJustificativa = "";
                var eChave = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da MDF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrMDFe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEncerrar_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var eChave = "";
                var cMunicipio = "";
                if (InputBox.Show("WebServices Eventos: Encerrar", "Chave da MDF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Encerrar", "Código do Municipio", ref cMunicipio) != DialogResult.OK) return;

                var ret = ACBrMDFe.EncerrarMDFe(eChave, DateTime.Now, cMunicipio);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

        private void btnIncCondutor_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var cOrgao = 0;
                var chave = "";
                var condutor = "";
                var cpf = "";
                var lote = 1;
                var cnpjcpf = "";
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Codigo da UF", ref cOrgao) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "CNPJCPF do emissor", ref cnpjcpf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Chave da MDF-e", ref chave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Nome do Condutor", ref condutor) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "CPF do Condutor", ref cpf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Inclusão de Condutor", "Número do Lote", ref lote) != DialogResult.OK) return;

                var evento = new EventoIncCondutor
                {
                    cOrgao = cOrgao,
                    chMDFe = chave,
                    xNome = condutor,
                    CPF = cpf,
                    dhEvento = DateTime.Now,
                    CNPJCPF = cnpjcpf
                };

                ACBrMDFe.LimparListaEventos();
                ACBrMDFe.CarregarEvento(evento);

                var ret = ACBrMDFe.EnviarEvento(lote);
                rtbRespostas.AppendLine(ret.Resposta);

            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEvento_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var idLote = 1;
                if (InputBox.Show("WebServices Eventos: Enviar", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.EnviarEvento(idLote);
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
                var arquivoIni = Helpers.OpenFile("Arquivo Ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrMDFe.CarregarEventoINI(arquivoIni);
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
                if (MessageBox.Show(@"Limpar a lista de eventos ?", @"ACBrLibMDFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrMDFe.LimparListaEventos();
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

                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.LimparListaEventos();
                ACBrMDFe.ImprimirEvento(arquivoXml, arquivoXmlEvento);
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

                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrMDFe.LimparListaEventos();
                ACBrMDFe.ImprimirEventoPDF(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmailEvento_Click(object sender, EventArgs e)
        {
            if (!validacaoEmail())
            {
                MessageBox.Show(@"Erro - Verifique as configurações de E-mail");
                return;
            }

            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrMDFe.EnviarEmailEvento(destinatario, arquivoXmlEvento, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorChave_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var codUf = 35;
                var cnpj = "";
                var chave = "";

                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Chave da MDFe", ref chave) != DialogResult.OK) return;

                var ret = ACBrMDFe.DistribuicaoDFePorChave(codUf, cnpj, chave);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorNSU_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrMDFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret.Resposta);
        }

        private void btnDFePorUltNSU_Click(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrMDFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret.Resposta);
        }

        #endregion EventHandlers

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void btnGerarChaveMDFe_Click(object sender, EventArgs e)
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

                rtbRespostas.AppendText(ACBrMDFe.GerarChave(uf, cod, doc, serie, numero, emissao, DateTime.Now, cnpjCPF));
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarAssincrono_Click_1(object sender, EventArgs e)
        {
            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                CheckMDFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote);
                rtbRespostas.AppendText(ret.Resposta);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarManifesto_Click(object sender, EventArgs e)
        {
            var mdfe = AlimendarDados();  //Método que Alimenta os dados nas classe e retorna o .ini de entrada

            /*string path = "C:\\Temp\\file.txt";
            using (StreamWriter writetext = new StreamWriter(path))
            {
                writetext.WriteLine(mdfe);
            }*/

            ACBrMDFe.LimparLista();
            ACBrMDFe.CarregarINI(mdfe);

            if (!validacao())
            {
                MessageBox.Show(@"Erro Verifique as configurações.");
                return;
            }

            try
            {
                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrMDFe.Enviar(aLote);

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

                foreach (var item in ret.Retorno.Items)
                {
                    rtbRespostas.AppendLine("[MDFe" + Convert.ToString(item.Numero) + "]");
                    rtbRespostas.AppendLine("id=" + Convert.ToString(item.Id));
                    rtbRespostas.AppendLine("tpAmbiente=" + Convert.ToString(item.tpAmb));
                    rtbRespostas.AppendLine("cStat=" + Convert.ToString(item.cStat));
                    rtbRespostas.AppendLine("xMotivo=" + Convert.ToString(item.xMotivo));
                    rtbRespostas.AppendLine("nProt=" + Convert.ToString(item.nProt));
                    rtbRespostas.AppendLine("chMDFe=" + Convert.ToString(item.chDFe));
                    rtbRespostas.AppendLine("digVal=" + Convert.ToString(item.digVal));
                    rtbRespostas.AppendLine("xml=" + Convert.ToString(item.XML));
                }
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

    }

}