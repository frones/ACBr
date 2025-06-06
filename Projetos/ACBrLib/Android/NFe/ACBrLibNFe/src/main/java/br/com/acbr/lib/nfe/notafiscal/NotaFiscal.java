package br.com.acbr.lib.nfe.notafiscal;

import static br.com.acbr.lib.comum.dfe.CSOSNIcms.csosnVazio;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import br.com.acbr.lib.comum.dfe.CSOSNIcms;
import br.com.acbr.lib.comum.dfe.CSTCofins;
import br.com.acbr.lib.comum.dfe.CSTIBSCBS;
import br.com.acbr.lib.comum.dfe.CSTIcms;
import br.com.acbr.lib.comum.dfe.CSTPIS;
import br.com.acbr.lib.comum.dfe.AutXML;
import br.com.acbr.lib.comum.dfe.InfRespTec;
import br.com.acbr.lib.comum.dfe.TcCredPres;
import br.com.acbr.lib.comum.dfe.gCBS;
import br.com.acbr.lib.comum.dfe.gCBSTot;
import br.com.acbr.lib.comum.dfe.gCredPres;
import br.com.acbr.lib.comum.dfe.gIBSCBS;
import br.com.acbr.lib.comum.dfe.gIBSMun;
import br.com.acbr.lib.comum.dfe.gIBSMunTot;
import br.com.acbr.lib.comum.dfe.gIBSUF;
import br.com.acbr.lib.comum.dfe.gIBSUFTot;
import br.com.acbr.lib.comum.dfe.gTribRegular;
import br.com.acbr.lib.comum.ini.ACBrIniFile;
import br.com.acbr.lib.comum.ini.IniUtil;
import br.com.acbr.lib.nfe.gIBSCBSMono;
import br.com.acbr.lib.nfe.gIBSCBSMonoTot;

public final class NotaFiscal {

    private CSTPIS[] CSTPISValues = { CSTPIS.pis04, CSTPIS.pis05, CSTPIS.pis06, CSTPIS.pis07, CSTPIS.pis08, CSTPIS.pis09 };
    private CSTCofins[] CSTCofinsValues = { CSTCofins.cof04, CSTCofins.cof05, CSTCofins.cof06, CSTCofins.cof07, CSTCofins.cof08, CSTCofins.cof09 };
    private CSTIcms[] CSTIcmsValues = {
            CSTIcms.cstVazio, CSTIcms.cst00, CSTIcms.cst01, CSTIcms.cst02, CSTIcms.cst10, CSTIcms.cst12,
            CSTIcms.cst13, CSTIcms.cst14, CSTIcms.cst15, CSTIcms.cst20, CSTIcms.cst21, CSTIcms.cst30,
            CSTIcms.cst40, CSTIcms.cst41, CSTIcms.cst45, CSTIcms.cst50, CSTIcms.cst51, CSTIcms.cst53,
            CSTIcms.cst60, CSTIcms.cst61, CSTIcms.cst70, CSTIcms.cst72, CSTIcms.cst73, CSTIcms.cst74,
            CSTIcms.cst80, CSTIcms.cst81, CSTIcms.cst90, CSTIcms.cstICMSOutraUF, CSTIcms.cstICMSSN,
            CSTIcms.cstPart10, CSTIcms.cstPart90, CSTIcms.cstRep41, CSTIcms.cstRep60
    };
    private CSOSNIcms[] CSOSNValues = {
            CSOSNIcms.csosnVazio, CSOSNIcms.csosn101, CSOSNIcms.csosn102, CSOSNIcms.csosn103, CSOSNIcms.csosn201,
            CSOSNIcms.csosn202, CSOSNIcms.csosn203, CSOSNIcms.csosn300, CSOSNIcms.csosn400, CSOSNIcms.csosn500, CSOSNIcms.csosn900
    };


    public ProcNFe ProcNFe;
    public InfNFe InfNFe;
    public IdentificacaoNFe Identificacao;
    public EmitenteNFe Emitente;
    public AvulsaNFe Avulsa;
    public DestinatarioNFe Destinatario;
    public RetiradaEntregaNFe Retirada;
    public RetiradaEntregaNFe Entrega;
    public List<AutXML> AutXML;
    public List<ProdutoNFe> Produtos;
    public TotalNFe Total;
    public ISSQNtotNFe ISSQNtot;
    public RetTribNFe RetTrib;
    public TransportadorNFe Transportador;
    public List<VolumeNFe> Volumes;
    public FaturaNFe Fatura;
    public List<DuplicataNFe> Duplicatas;
    public List<PagamentoNFe> Pagamentos;
    public InfIntermedNFe InfIntermed;
    public DadosAdicionaisNFe DadosAdicionais;
    public ExportaNFe Exporta;
    public CompraNFe Compra;
    public CanaNFe Cana;
    public InfNFeSupl InfNFeSupl;
    public InfRespTec InfRespTec;

    public NotaFiscal() {
        ProcNFe = new ProcNFe();
        InfNFe = new InfNFe();
        Identificacao = new IdentificacaoNFe();
        Emitente = new EmitenteNFe();
        Avulsa = new AvulsaNFe();
        Destinatario = new DestinatarioNFe();
        Retirada = new RetiradaEntregaNFe();
        Entrega = new RetiradaEntregaNFe();
        AutXML = new ArrayList<>(10);
        Produtos = new ArrayList<>(990);
        Total = new TotalNFe();
        ISSQNtot = new ISSQNtotNFe();
        RetTrib = new RetTribNFe();
        Transportador = new TransportadorNFe();
        Volumes = new ArrayList<>();
        Fatura = new FaturaNFe();
        Duplicatas = new ArrayList<>();
        Pagamentos = new ArrayList<>();
        InfIntermed = new InfIntermedNFe();
        DadosAdicionais = new DadosAdicionaisNFe();
        Exporta = new ExportaNFe();
        Compra = new CompraNFe();
        Cana = new CanaNFe();
        InfNFeSupl = new InfNFeSupl();
        InfRespTec = new InfRespTec();

        InfNFe.setVersao("4.00");
    }

    public NotaFiscal(ACBrIniFile iniData) {
        this();
        readFromIni(iniData);
    }

    private void readFromIni(ACBrIniFile iniData) {
        ProcNFe procNFe = IniUtil.readFromIni(iniData, ProcNFe.class, "procNFe");
        InfNFe infNFe = IniUtil.readFromIni(iniData, InfNFe.class, "infNFe");
        IdentificacaoNFe identificacao = IniUtil.readFromIni(iniData, IdentificacaoNFe.class, "Identificacao");

        int i = 0;
        NFRef nfRef;
        while ((nfRef = IniUtil.readFromIni(iniData, NFRef.class, String.format("NFRef%03d", ++i))) != null) {
            identificacao.getNFref().add(nfRef);
        }

        EmitenteNFe emitente = IniUtil.readFromIni(iniData, EmitenteNFe.class, "Emitente");
        AvulsaNFe avulsa = IniUtil.readFromIni(iniData, AvulsaNFe.class, "Avulsa");
        DestinatarioNFe destinatario = IniUtil.readFromIni(iniData, DestinatarioNFe.class, "Destinatario");
        RetiradaEntregaNFe retirada = IniUtil.readFromIni(iniData, RetiradaEntregaNFe.class, "Retirada");
        RetiradaEntregaNFe entrega = IniUtil.readFromIni(iniData, RetiradaEntregaNFe.class, "Entrega");

        i = 0;
        AutXML autXml;
        while ((autXml = IniUtil.readFromIni(iniData, AutXML.class, String.format("autXML%02d", ++i))) != null) {
            AutXML.add(autXml);
        }

        i = 0;
        ProdutoNFe produto = new ProdutoNFe();
        do{

            i++;
            produto = IniUtil.readFromIni(iniData, ProdutoNFe.class, String.format("Produto%03d", i));
            if (produto == null) continue;

            int k = 0;
            NVENFe nveItem;
            do {
                k++;
                nveItem = IniUtil.readFromIni(iniData, NVENFe.class, String.format("NVE%03d%03d", i, k));
                if (nveItem != null) {
                    produto.getNVE().add(nveItem);
                }
            } while (nveItem != null);

            k = 0;
            DINFe diItem;
            do {
                k++;
                diItem = IniUtil.readFromIni(iniData, DINFe.class, String.format("DI%03d%03d", i, k));
                if (diItem == null) continue;

                int j = 0;
                LADINFe ladiItem;
                do {
                    j++;
                    ladiItem = IniUtil.readFromIni(iniData, LADINFe.class, String.format("DI%03d%03d%03d", i, k, j));
                    if (ladiItem != null) {
                        diItem.getLadi().add(ladiItem);
                    }
                } while (ladiItem != null);

                produto.getDI().add(diItem);
            } while (diItem != null);

            k = 0;
            DetExportNFe detExport;
            do {
                k++;
                detExport = IniUtil.readFromIni(iniData, DetExportNFe.class, String.format("detExport%03d%03d", i, k));
                if (detExport != null) {
                    produto.getDetExport().add(detExport);
                }
            } while (detExport != null);

            k = 0;
            RastroNFe rastroItem;
            do {
                k++;
                rastroItem = IniUtil.readFromIni(iniData, RastroNFe.class, String.format("Rastro%03d%03d", i, k));
                if (rastroItem != null) {
                    produto.getRastro().add(rastroItem);
                }
            } while (rastroItem != null);

            k = 0;
            MedicamentoNFe medItem;
            do {
                k++;
                medItem = IniUtil.readFromIni(iniData, MedicamentoNFe.class, String.format("Medicamento%03d%03d", i, k));
                if (medItem != null) {
                    produto.getMedicamento().add(medItem);
                }
            } while (medItem != null);

            k = 0;
            ArmaNFe armaItem;
            do {
                k++;
                armaItem = IniUtil.readFromIni(iniData, ArmaNFe.class, String.format("Arma%03d%03d", i, k));
                if (armaItem != null) {
                    produto.getArma().add(armaItem);
                }
            } while (armaItem != null);

            ImpostoDevolNFe impostoDevol = produto.getImpostoDevol();
            if (impostoDevol == null) {
                impostoDevol = new ImpostoDevolNFe();
                produto.setImpostoDevol(impostoDevol);
            }
            IniUtil.readFromIni(iniData, ImpostoDevolNFe.class, String.format("impostoDevol%03d", i));

            VeiculoNFe veiculo = produto.getVeiculo();
            if (veiculo == null) {
                veiculo = new VeiculoNFe();
                produto.setVeiculo(veiculo);
            }
            IniUtil.readFromIni(iniData, VeiculoNFe.class, String.format("Veiculo%03d", i));

            CombustivelNFe combustivel = produto.getCombustivel();
            if (combustivel == null) {
                combustivel = new CombustivelNFe();
                produto.setCombustivel(combustivel);
            }
            IniUtil.readFromIni(iniData, CombustivelNFe.class, String.format("Combustivel%03d", i));

            CIDENFe cide = combustivel.getCIDE();
            if (cide == null) {
                cide = new CIDENFe();
                combustivel.setCIDE(cide);
            }
            IniUtil.readFromIni(iniData, CIDENFe.class, String.format("CIDE%03d", i));

            EncerranteNFe encerrante = combustivel.getEncerrante();
            if (encerrante == null) {
                encerrante = new EncerranteNFe();
                combustivel.setEncerrante(encerrante);
            }
            IniUtil.readFromIni(iniData, EncerranteNFe.class, String.format("encerrante%03d", i));

            DFeReferenciado dfeReferenciado = produto.getDFeReferenciado();
            if(dfeReferenciado == null){
                dfeReferenciado = new DFeReferenciado();
                produto.setDFeReferenciado(dfeReferenciado);
            }
            IniUtil.readFromIni(iniData, DFeReferenciado.class, String.format("DFeReferenciado%03d", i));

            k = 0;
            OrigCombNFe origComb;
            do {
                k++;
                origComb = IniUtil.readFromIni(iniData, OrigCombNFe.class, String.format("origComb%03d%02d", i, k));
                if (origComb != null) {
                    produto.getCombustivel().getOrigComb().add(origComb);
                }
            } while (origComb != null);

            ICMSProdutoNFe icms = produto.getICMS();
            if (icms == null) {
                icms = new ICMSProdutoNFe();
                produto.setICMS(icms);
            }
            IniUtil.readFromIni(iniData, ICMSProdutoNFe.class, String.format("ICMS%03d", i));

            IPIProdutoNFe ipi = produto.getIPI();
            if (ipi == null) {
                ipi = new IPIProdutoNFe();
                produto.setIPI(ipi);
            }
            IniUtil.readFromIni(iniData, IPIProdutoNFe.class, String.format("IPI%03d", i));

            IIProdutoNFe ii = produto.getII();
            if (ii == null) {
                ii = new IIProdutoNFe();
                produto.setII(ii);
            }
            IniUtil.readFromIni(iniData, IIProdutoNFe.class, String.format("II%03d", i));

            PISProdutoNFe pis = produto.getPIS();
            if (pis == null) {
                pis = new PISProdutoNFe();
                produto.setPIS(pis);
            }
            IniUtil.readFromIni(iniData, PISProdutoNFe.class, String.format("PIS%03d", i));

            PISSTProdutoNFe pisst = produto.getPISST();
            if (pisst == null) {
                pisst = new PISSTProdutoNFe();
                produto.setPISST(pisst);
            }
            IniUtil.readFromIni(iniData, PISSTProdutoNFe.class, String.format("PISST%03d", i));

            COFINSProdutoNFe cofins = produto.getCOFINS();
            if (cofins == null) {
                cofins = new COFINSProdutoNFe();
                produto.setCOFINS(cofins);
            }
            IniUtil.readFromIni(iniData, COFINSProdutoNFe.class, String.format("COFINS%03d", i));

            COFINSSTProdutoNFe cofinsst = produto.getCOFINSST();
            if (cofinsst == null) {
                cofinsst = new COFINSSTProdutoNFe();
                produto.setCOFINSST(cofinsst);
            }
            IniUtil.readFromIni(iniData, COFINSSTProdutoNFe.class, String.format("COFINSST%03d", i));

            ISSQNNFe issqn = produto.getISSQN();
            if (issqn == null) {
                issqn = new ISSQNNFe();
                produto.setISSQN(issqn);
            }
            IniUtil.readFromIni(iniData, ISSQNNFe.class, String.format("ISSQN%03d", i));

            ISProdutoNFe is = produto.getIS();
            if(is == null){
                is = new ISProdutoNFe();
                produto.setIS(is);
            }
            IniUtil.readFromIni(iniData, ISProdutoNFe.class, String.format("IS%03d", i));

            IBSCBSProdutoNFe ibscbs = produto.getIBSCBS();
            if(ibscbs == null){
                ibscbs = new IBSCBSProdutoNFe();
                produto.setIBSCBS(ibscbs);
            }
            IniUtil.readFromIni(iniData, IBSCBSProdutoNFe.class, String.format("IBSCBS%03d", i));

            gIBSCBS gIBSCBS = produto.getIBSCBS().getgIBSCBS();
            if(gIBSCBS == null){
                gIBSCBS = new gIBSCBS();
                produto.getIBSCBS().setgIBSCBS(gIBSCBS);
            }
            IniUtil.readFromIni(iniData, gIBSCBS.class, String.format("gIBSCBS%03d", i));

            gIBSUF gIBSUF = produto.getIBSCBS().getgIBSCBS().getgIBSUF();
            if(gIBSUF == null){
                gIBSUF = new gIBSUF();
                produto.getIBSCBS().getgIBSCBS().setgIBSUF(gIBSUF);
            }
            IniUtil.readFromIni(iniData, gIBSUF.class, String.format("gIBSUF%03d", i));

            gIBSMun gIBSMun = produto.getIBSCBS().getgIBSCBS().getgIBSMun();
            if(gIBSMun == null){
                gIBSMun = new gIBSMun();
                produto.getIBSCBS().getgIBSCBS().setgIBSMun(gIBSMun);
            }
            IniUtil.readFromIni(iniData, gIBSMun.class, String.format("gIBSMun%03d", i));

            gCBS gCBS = produto.getIBSCBS().getgIBSCBS().getgCBS();
            if(gCBS == null){
                gCBS = new gCBS();
                produto.getIBSCBS().getgIBSCBS().setgCBS(gCBS);
            }
            IniUtil.readFromIni(iniData, gCBS.class, String.format("gCBS%03d", i));

            gTribRegular gTribRegular = produto.getIBSCBS().getgIBSCBS().getgTribRegular();
            if(gTribRegular == null){
                gTribRegular = new gTribRegular();
                produto.getIBSCBS().getgIBSCBS().setgTribRegular(gTribRegular);
            }
            IniUtil.readFromIni(iniData, gTribRegular.class, String.format("gTribRegular%03d", i));

            gCredPres gIBSCredPres = produto.getIBSCBS().getgIBSCBS().getgIBSCredPres();
            if(gIBSCredPres == null){
                gIBSCredPres = new gCredPres();
                produto.getIBSCBS().getgIBSCBS().setgIBSCredPres(gIBSCredPres);
            }
            IniUtil.readFromIni(iniData, gCredPres.class, String.format("gIBSCredPres%03d", i));

            gCredPres gCBSCredPres = produto.getIBSCBS().getgIBSCBS().getgCBSCredPres();
            if(gCBSCredPres == null){
                gCBSCredPres = new gCredPres();
                produto.getIBSCBS().getgIBSCBS().setgCBSCredPres(gCBSCredPres);
            }
            IniUtil.readFromIni(iniData, gCredPres.class, String.format("gCBSCredPres%03d", i));

            gIBSCBSMono gIBSCBSMono = produto.getIBSCBS().getgIBSCBSMono();
            if(gIBSCBSMono == null){
                gIBSCBSMono = new gIBSCBSMono();
                produto.getIBSCBS().setgIBSCBSMono(gIBSCBSMono);
            }
            IniUtil.readFromIni(iniData, gIBSCBSMono.class, String.format("gIBSCBSMono%03d", i));

            TotalNFe total = IniUtil.readFromIni(iniData, TotalNFe.class, "Total");
            ISSQNtotNFe issqnTot = IniUtil.readFromIni(iniData, ISSQNtotNFe.class, "ISSQNtot");
            ISTotNFe ISTot = IniUtil.readFromIni(iniData, ISTotNFe.class, "ISTot");
            IBSCBSTotNFe IBSCBSTot = IniUtil.readFromIni(iniData, IBSCBSTotNFe.class, "IBSCBSTot");
            IBSCBSTotNFe gIBS = IniUtil.readFromIni(iniData, IBSCBSTotNFe.class, "gIBS");
            gIBSUFTot gIBSUFTot = IniUtil.readFromIni(iniData, gIBSUFTot.class, "gIBSUFTot");
            gIBSMunTot gIBSMunTot = IniUtil.readFromIni(iniData, gIBSMunTot.class, "gIBSMunTot");
            gCBSTot gCBSTot = IniUtil.readFromIni(iniData, gCBSTot.class, "gCBSTot");
            gIBSCBSMonoTot gIBSCBSMonoTot = IniUtil.readFromIni(iniData, gIBSCBSMonoTot.class, "gMono");

            RetTribNFe retTrib = IniUtil.readFromIni(iniData, RetTribNFe.class, "retTrib");
            TransportadorNFe transportador = IniUtil.readFromIni(iniData, TransportadorNFe.class, "Transportador");

            i = 0;
            ReboqueNFe reboque;
            do {
                i++;
                reboque = IniUtil.readFromIni(iniData, ReboqueNFe.class, String.format("Reboque%03d", i));
                if (reboque == null) continue;

                Transportador.getReboque().add(reboque);
            } while (reboque != null);

            i = 0;
            VolumeNFe volume;
            do {
                i++;
                volume = IniUtil.readFromIni(iniData, VolumeNFe.class, String.format("Volume%03d", i));
                if (volume == null) continue;

                k = 0;
                LacresNFe lacre;

                do {
                    k++;
                    lacre = IniUtil.readFromIni(iniData, LacresNFe.class, String.format("Lacre%03d%03d", i, k));
                    if (lacre == null) continue;

                    volume.getLacres().add(lacre);
                } while (lacre != null);

                Volumes.add(volume);
            } while (volume != null);

            FaturaNFe fatura = IniUtil.readFromIni(iniData, FaturaNFe.class, "Fatura");

            i = 0;
            DuplicataNFe duplicata;
            do {
                i++;
                duplicata = IniUtil.readFromIni(iniData, DuplicataNFe.class, String.format("Duplicata%03d", i));
                if (duplicata == null) continue;

                Duplicatas.add(duplicata);
            } while (duplicata != null);

            i = 0;
            PagamentoNFe pag;
            do {
                i++;
                pag = IniUtil.readFromIni(iniData, PagamentoNFe.class, String.format("pag%03d", i));
                if (pag == null) continue;

                Pagamentos.add(pag);
            } while (pag != null);

            DadosAdicionaisNFe dadosAdicionais = IniUtil.readFromIni(iniData, DadosAdicionaisNFe.class, "DadosAdicionais");

            i = 0;
            InfoAdicionalNfe info;

            do {
                i++;
                info = IniUtil.readFromIni(iniData, InfoAdicionalNfe.class, String.format("obsCont%03d", i));
                if (info == null) continue;

                DadosAdicionais.getObsCont().add(info);
            } while (info != null);

            i = 0;

            do {
                i++;
                info = IniUtil.readFromIni(iniData, InfoAdicionalNfe.class, String.format("obsFisco%03d", i));
                if (info == null) continue;

                DadosAdicionais.getObsFisco().add(info);
            } while (info != null);

            i = 0;
            ProcRefNFe procRef;

            do {
                i++;
                procRef = IniUtil.readFromIni(iniData, ProcRefNFe.class, String.format("procRef%03d", i));
                if (procRef == null) continue;

                DadosAdicionais.getProcRef().add(procRef);
            } while (procRef != null);

            ExportaNFe exporta = IniUtil.readFromIni(iniData, ExportaNFe.class, "Exporta");
            CompraNFe compra = IniUtil.readFromIni(iniData, CompraNFe.class, "compra");
            CanaNFe cana = IniUtil.readFromIni(iniData, CanaNFe.class, "cana");

            i = 0;
            ForDiaNFe forDia;

            do {
                i++;
                forDia = IniUtil.readFromIni(iniData, ForDiaNFe.class, String.format("forDia%03d", i));
                if (forDia == null) continue;

                Cana.getForDia().add(forDia);
            } while (forDia != null);

            i = 0;
            DeducNFe deduc;

            do {
                i++;
                deduc = IniUtil.readFromIni(iniData, DeducNFe.class, String.format("deduc%03d", i));
                if (deduc == null) continue;

                Cana.getDeduc().add(deduc);
            } while (deduc != null);

            InfNFeSupl infNFeSupl = IniUtil.readFromIni(iniData, InfNFeSupl.class, "infNFeSupl");
            InfRespTec infRespTec = IniUtil.readFromIni(iniData, InfRespTec.class, "infRespTec");

            Produtos.add(produto);
        } while (produto != null);
    }

    @Override
    public String toString() {
        return writeToIni().toString();
    }

    private ACBrIniFile writeToIni(){
        ACBrIniFile iniData = new ACBrIniFile();

        if (ProcNFe.getNProt() != null && !ProcNFe.getNProt().isEmpty()) {
            IniUtil.writeToIni(iniData, ProcNFe, "procNFe");
        }

        IniUtil.writeToIni(iniData, InfNFe, "infNFe");
        IniUtil.writeToIni(iniData, Identificacao, "Identificacao");

        for (int i = 0; i < Identificacao.getNFref().size(); i++) {
            IniUtil.writeToIni(iniData, Identificacao.getNFref().get(i), String.format("NFRef%03d", i + 1));
        }

        IniUtil.writeToIni(iniData, Emitente, "Emitente");

        if (Avulsa.getCnpj() != null && !Avulsa.getCnpj().isEmpty()) {
            IniUtil.writeToIni(iniData, Avulsa, "Avulsa");
        }

        IniUtil.writeToIni(iniData, Destinatario, "Destinatario");

        if (Retirada.getXLgr() != null && !Retirada.getXLgr().isEmpty()) {
            IniUtil.writeToIni(iniData, Retirada, "Retirada");
        }

        if (Entrega.getXLgr() != null && !Entrega.getXLgr().isEmpty()) {
            IniUtil.writeToIni(iniData, Entrega, "Entrega");
        }

        for (int i = 0; i < AutXML.size(); i++) {
            IniUtil.writeToIni(iniData, AutXML.get(i), String.format("autXML%02d", i + 1));
        }

        for (int i = 0; i < Produtos.size(); i++) {
            ProdutoNFe produto = Produtos.get(i);

            IniUtil.writeToIni(iniData, produto, String.format("Produto%03d", i + 1));

            for (int k = 0; k < produto.getNVE().size(); k++) {
                IniUtil.writeToIni(iniData, produto.getNVE().get(k), String.format("NVE%03d%03d", i + 1, k + 1));
            }

            for (int k = 0; k < produto.getDI().size(); k++) {
                IniUtil.writeToIni(iniData, produto.getDI().get(k), String.format("DI%03d%03d", i + 1, k + 1));

                for (int j = 0; j < produto.getDI().get(k).getLadi().size(); j++) {
                    IniUtil.writeToIni(iniData, produto.getDI().get(k).getLadi().get(j), String.format("LADI%03d%03d%03d", i + 1, k + 1, j + 1));
                }
            }

            for (int k = 0; k < produto.getDetExport().size(); k++) {
                IniUtil.writeToIni(iniData, produto.getDetExport().get(k), String.format("detExport%03d%03d", i + 1, k + 1));
            }

            for (int k = 0; k < produto.getRastro().size(); k++) {
                IniUtil.writeToIni(iniData, produto.getRastro().get(k), String.format("rastro%03d%03d", i + 1, k + 1));
            }

            for (int k = 0; k < produto.getMedicamento().size(); k++) {
                IniUtil.writeToIni(iniData, produto.getMedicamento().get(k), String.format("Medicamento%03d%03d", i + 1, k + 1));
            }

            for (int k = 0; k < produto.getArma().size(); k++) {
                IniUtil.writeToIni(iniData, produto.getArma().get(k), String.format("Arma%03d%03d", i + 1, k + 1));
            }

            if (produto.getImpostoDevol().getPDevol() > 0) {
                IniUtil.writeToIni(iniData, produto.getImpostoDevol(), String.format("impostoDevol%03d", i + 1));
            }

            if (produto.getVeiculo().getChassi() != null && !produto.getVeiculo().getChassi().isEmpty()) {
                IniUtil.writeToIni(iniData, produto.getVeiculo(), String.format("Veiculo%03d", i + 1));
            }

            if (produto.getCombustivel().getCProdANP() > 0) {
                IniUtil.writeToIni(iniData, produto.getCombustivel(), String.format("Combustivel%03d", i + 1));

                if (produto.getCombustivel().getCIDE().getQBCProd() > 0 || produto.getCombustivel().getCIDE().getVAliqProd() > 0 || produto.getCombustivel().getCIDE().getVCIDE() > 0) {
                    IniUtil.writeToIni(iniData, produto.getCombustivel().getCIDE(), String.format("CIDE%03d", i + 1));
                }

                if (produto.getCombustivel().getEncerrante().getNBico() > 0) {
                    IniUtil.writeToIni(iniData, produto.getCombustivel().getEncerrante(), String.format("encerrante%03d", i + 1));
                }

                if (produto.getCombustivel().getOrigComb().size() > 0) {
                    for (int k = 0; k < produto.getCombustivel().getOrigComb().size(); k++) {
                        IniUtil.writeToIni(iniData, produto.getCombustivel().getOrigComb().get(k), String.format("origComb%03d%02d", i + 1, k + 1));
                    }
                }
            }

            if(produto.getDFeReferenciado().getChaveAcesso() != ""){
                IniUtil.writeToIni(iniData, produto.getDFeReferenciado(), String.format("DFeReferenciado%03d", i + 1));
            }

            IniUtil.writeToIni(iniData, produto.getICMS(), String.format("ICMS%03d", i + 1));

            if (produto.getICMSUFDEST().getPICMSInterPart() != null) {
                IniUtil.writeToIni(iniData, produto.getICMSUFDEST(), String.format("ICMSUFDEST%03d", i + 1));
            }

            if (produto.getIPI().getCEnq() != null && !produto.getIPI().getCEnq().isEmpty()) {
                IniUtil.writeToIni(iniData, produto.getIPI(), String.format("IPI%03d", i + 1));
            }

            if (produto.getII().getVBC() != null) {
                IniUtil.writeToIni(iniData, produto.getII(), String.format("II%03d", i + 1));
            }

            if (produto.getPIS().getVBC() != null || Arrays.asList(CSTPISValues).contains(produto.getPIS().getCST())) {
                IniUtil.writeToIni(iniData, produto.getPIS(), String.format("PIS%03d", i + 1));
            }

            if (produto.getPISST().getVBC() != null) {
                IniUtil.writeToIni(iniData, produto.getPISST(), String.format("PISST%03d", i + 1));
            }

            if (produto.getCOFINS().getvBC() != null || Arrays.asList(CSTCofinsValues).contains(produto.getCOFINS().getCst())) {
                IniUtil.writeToIni(iniData, produto.getCOFINS(), String.format("COFINS%03d", i + 1));
            }

            if (produto.getCOFINSST().getvBC() != null) {
                IniUtil.writeToIni(iniData, produto.getCOFINSST(), String.format("COFINSST%03d", i + 1));
            }

            if (produto.getISSQN().getvBC() != null) {
                IniUtil.writeToIni(iniData, produto.getISSQN(), String.format("ISSQN%03d", i + 1));
            }

            if (produto.getIS().getCSTIS() != CSTIS.cstisNenhum){
                IniUtil.writeToIni(iniData, produto.getIS(), String.format("IS%03d", i + 1));
            }

            if (produto.getIBSCBS().getCST() != CSTIBSCBS.cstVazio){
                IniUtil.writeToIni(iniData, produto.getIBSCBS(), String.format("IBSCBS%03d", i + 1));

                if (produto.getIBSCBS().getgIBSCBS().getvBC().compareTo(BigDecimal.ZERO) > 0){
                    IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS(), String.format("gIBSCBS%03d", i + 1));
                    IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS().getgIBSUF(), String.format("gIBSUF%03d", i + 1));
                    IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS().getgIBSMun(), String.format("gIBSMun%03d", i + 1));
                    IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS().getgCBS(), String.format("gCBS%03d", i + 1));

                    if (produto.getIBSCBS().getgIBSCBS().getgTribRegular().getCSTReg() != CSTIBSCBS.cstVazio){
                        IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS().getgTribRegular(), String.format("gTribRegular%03d", i + 1));
                    }

                    if(produto.getIBSCBS().getgIBSCBS().getgIBSCredPres().getcCredPres() != TcCredPres.cpNenhum){
                        IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS().getgIBSCredPres(), String.format("gIBSCredPres%03d", i + 1));
                    }

                    if(produto.getIBSCBS().getgIBSCBS().getgCBSCredPres().getcCredPres() != TcCredPres.cpNenhum){
                        IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBS().getgCBSCredPres(), String.format("gCBSCredPres%03d", i + 1));
                    }

                }
                else if (produto.getIBSCBS().getgIBSCBSMono().getAdRemIBS().compareTo(BigDecimal.ZERO) > 0){
                    IniUtil.writeToIni(iniData, produto.getIBSCBS().getgIBSCBSMono(), String.format("gIBSCBSMono%03d", i + 1));
                }
                else{
                    if(produto.getIBSCBS().getCST() == CSTIBSCBS.cst800){
                        IniUtil.writeToIni(iniData, produto.getIBSCBS().getgTransfCred(), String.format("gTransfCred%03d", i + 1));
                    }
                }
            }
        }

        IniUtil.writeToIni(iniData, Total, "Total");

        if (ISSQNtot.getVBC() != null) {
            IniUtil.writeToIni(iniData, ISSQNtot, "ISSQNtot");
        }

        IniUtil.writeToIni(iniData, Total.getISTot(), "ISTot");
        IniUtil.writeToIni(iniData, Total.getIBSCBSTot(), "IBSCBSTot");
        IniUtil.writeToIni(iniData, Total.getIBSCBSTot().getgIBS(), "gIBS");
        IniUtil.writeToIni(iniData, Total.getIBSCBSTot().getgIBS().getgIBSUF(), "gIBSUFTot");
        IniUtil.writeToIni(iniData, Total.getIBSCBSTot().getgIBS().getgIBSMun(), "gIBSMunTot");
        IniUtil.writeToIni(iniData, Total.getIBSCBSTot().getgCBS(), "gCBSTot");
        IniUtil.writeToIni(iniData, Total.getIBSCBSTot().getgMono(), "gMono");

        IniUtil.writeToIni(iniData, RetTrib, "retTrib");

        IniUtil.writeToIni(iniData, Transportador, "Transportador");

        for (int i = 0; i < Transportador.getReboque().size(); i++) {
            IniUtil.writeToIni(iniData, Transportador.getReboque().get(i), String.format("Reboque%03d", i + 1));
        }

        for (int i = 0; i < Volumes.size(); i++) {
            IniUtil.writeToIni(iniData, Volumes.get(i), String.format("Volume%03d", i + 1));

            for (int k = 0; k < Volumes.get(i).getLacres().size(); k++) {
                IniUtil.writeToIni(iniData, Volumes.get(i).getLacres().get(k), String.format("Lacre%03d%03d", i + 1, k + 1));
            }
        }

        IniUtil.writeToIni(iniData, Fatura, "Fatura");

        for (int i = 0; i < Duplicatas.size(); i++) {
            IniUtil.writeToIni(iniData, Duplicatas.get(i), String.format("Duplicata%03d", i + 1));
        }

        for (int i = 0; i < Pagamentos.size(); i++) {
            IniUtil.writeToIni(iniData, Pagamentos.get(i), String.format("pag%03d", i + 1));
        }

        if (InfIntermed.getCNPJ() != null && !InfIntermed.getCNPJ().isEmpty()) {
            IniUtil.writeToIni(iniData, InfIntermed, "infIntermed");
        }

        IniUtil.writeToIni(iniData, DadosAdicionais, "DadosAdicionais");

        for (int i = 0; i < DadosAdicionais.getObsCont().size(); i++) {
            IniUtil.writeToIni(iniData, DadosAdicionais.getObsCont().get(i), String.format("obsCont%03d", i + 1));
        }

        for (int i = 0; i < DadosAdicionais.getObsFisco().size(); i++) {
            IniUtil.writeToIni(iniData, DadosAdicionais.getObsFisco().get(i), String.format("obsFisco%03d", i + 1));
        }

        for (int i = 0; i < DadosAdicionais.getProcRef().size(); i++) {
            IniUtil.writeToIni(iniData, DadosAdicionais.getProcRef().get(i), String.format("procRef%03d", i + 1));
        }

        if (Exporta.getUFSaidaPais() != null && !Exporta.getUFSaidaPais().isEmpty()) {
            IniUtil.writeToIni(iniData, Exporta, "Exporta");
        }

        if ((Compra.getXNEmp() != null && !Compra.getXNEmp().isEmpty()) ||
                (Compra.getXCont() != null && !Compra.getXCont().isEmpty()) ||
                (Compra.getXPed() != null && !Compra.getXPed().isEmpty())) {
            IniUtil.writeToIni(iniData, Compra, "compra");
        }

        if (Cana.getSafra() != null && !Cana.getSafra().isEmpty()) {
            IniUtil.writeToIni(iniData, Cana, "cana");

            for (int i = 0; i < Cana.getForDia().size(); i++) {
                IniUtil.writeToIni(iniData, Cana.getForDia().get(i), String.format("forDia%03d", i + 1));
            }

            for (int i = 0; i < Cana.getDeduc().size(); i++) {
                IniUtil.writeToIni(iniData, Cana.getDeduc().get(i), String.format("deduc%03d", i + 1));
            }
        }

        IniUtil.writeToIni(iniData, InfNFeSupl, "infNFeSupl");

        if (InfRespTec.getCnpj() != null && !InfRespTec.getCnpj().isEmpty()) {
            IniUtil.writeToIni(iniData, InfRespTec, "infRespTec");
        }

        return iniData;
    }
}
