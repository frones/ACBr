package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import br.com.acbr.lib.comum.dfe.IndIncentivo;

public class ProdutoNFe {

    public ProdutoNFe() {
        NVE = new ArrayList<>();
        DI = new ArrayList<>();
        DetExport = new ArrayList<>();
        Rastro = new ArrayList<>();
        Medicamento = new ArrayList<>();
        Arma = new ArrayList<>();
        ImpostoDevol = new ImpostoDevolNFe();
        Veiculo = new VeiculoNFe();
        Combustivel = new CombustivelNFe();
        ICMS = new ICMSProdutoNFe();
        ICMSUFDEST = new ICMSUFDESTNFe();
        IPI = new IPIProdutoNFe();
        II = new IIProdutoNFe();
        PIS = new PISProdutoNFe();
        PISST = new PISSTProdutoNFe();
        COFINS = new COFINSProdutoNFe();
        COFINSST = new COFINSSTProdutoNFe();
        ISSQN = new ISSQNNFe();
        gCred = new CreditoPresumidoNFe();
        IS = new ISProdutoNFe();
        IBSCBS = new IBSCBSProdutoNFe();
        DFeReferenciado = new DFeReferenciado();
    }

    private int nItem;
    private String cProd;
    private String cEAN;
    private String cEANTrib;
    private String cBarra;
    private String cBarraTrib;
    private String xProd;
    private String NCM;
    private String CEST;
    private String EXTIPI;
    private String CFOP;
    private String uCom;
    private double qCom;
    private double vUnCom;
    private double vProd;
    private String uTrib;
    private double qTrib;
    private double vUnTrib;
    private double vFrete;
    private double vSeg;
    private double vDesc;
    private double vOutro;
    private IndicadorTotal indTot;
    private String xPed;
    private int nItemPed;
    private String nFCI;
    private String nRECOPI;
    private double pDevol;
    private double vIPIDevol;
    private double vTotTrib;
    private String infAdProd;
    private IndEscala indEscala;
    private String CNPJFab;
    private String cBenef;
    private CreditoPresumidoNFe gCred;
    private List<NVENFe> NVE;
    private List<DINFe> DI;
    private List<DetExportNFe> DetExport;
    private List<RastroNFe> Rastro;
    private List<MedicamentoNFe> Medicamento;
    private List<ArmaNFe> Arma;
    private ImpostoDevolNFe ImpostoDevol;
    private VeiculoNFe Veiculo;
    private CombustivelNFe Combustivel;
    private ICMSProdutoNFe ICMS;
    private ICMSUFDESTNFe ICMSUFDEST;
    private IPIProdutoNFe IPI;
    private IIProdutoNFe II;
    private PISProdutoNFe PIS;
    private PISSTProdutoNFe PISST;
    private COFINSProdutoNFe COFINS;
    private COFINSSTProdutoNFe COFINSST;
    private ISSQNNFe ISSQN;
    private ISProdutoNFe IS;
    private IBSCBSProdutoNFe IBSCBS;
    private DFeReferenciado DFeReferenciado;
    private BigDecimal vItem;
    private IndBemMovelUsado IndBemMovelUsado;


    public int getnItem() { return nItem; }
    public void setnItem(int nItem) { this.nItem = nItem; }

    public String getcProd() { return cProd; }
    public void setcProd(String cProd) { this.cProd = cProd; }

    public String getcEAN() { return cEAN; }
    public void setcEAN(String cEAN) { this.cEAN = cEAN; }

    public String getcEANTrib() { return cEANTrib; }
    public void setcEANTrib(String cEANTrib) { this.cEANTrib = cEANTrib; }

    public String getcBarra() { return cBarra; }
    public void setcBarra(String cBarra) { this.cBarra = cBarra; }

    public String getcBarraTrib() { return cBarraTrib; }
    public void setcBarraTrib(String cBarraTrib) { this.cBarraTrib = cBarraTrib; }

    public String getxProd() { return xProd; }
    public void setxProd(String xProd) { this.xProd = xProd; }

    public String getNCM() { return NCM; }
    public void setNCM(String NCM) { this.NCM = NCM; }

    public String getCEST() { return CEST; }
    public void setCEST(String CEST) { this.CEST = CEST; }

    public String getEXTIPI() { return EXTIPI; }
    public void setEXTIPI(String EXTIPI) { this.EXTIPI = EXTIPI; }

    public String getCFOP() { return CFOP; }
    public void setCFOP(String CFOP) { this.CFOP = CFOP; }

    public String getuCom() { return uCom; }
    public void setuCom(String uCom) { this.uCom = uCom; }

    public double getqCom() { return qCom; }
    public void setqCom(double qCom) { this.qCom = qCom; }

    public double getvUnCom() { return vUnCom; }
    public void setvUnCom(double vUnCom) { this.vUnCom = vUnCom; }

    public double getvProd() { return vProd; }
    public void setvProd(double vProd) { this.vProd = vProd; }

    public String getuTrib() { return uTrib; }
    public void setuTrib(String uTrib) { this.uTrib = uTrib; }

    public double getqTrib() { return qTrib; }
    public void setqTrib(double qTrib) { this.qTrib = qTrib; }

    public double getvUnTrib() { return vUnTrib; }
    public void setvUnTrib(double vUnTrib) { this.vUnTrib = vUnTrib; }

    public double getvFrete() { return vFrete; }
    public void setvFrete(double vFrete) { this.vFrete = vFrete; }

    public double getvSeg() { return vSeg; }
    public void setvSeg(double vSeg) { this.vSeg = vSeg; }

    public double getvDesc() { return vDesc; }
    public void setvDesc(double vDesc) { this.vDesc = vDesc; }

    public double getvOutro() { return vOutro; }
    public void setvOutro(double vOutro) { this.vOutro = vOutro; }

    public IndicadorTotal getIndTot() { return indTot; }
    public void setIndTot(IndicadorTotal indTot) {
        if (indTot != null){
            this.indTot = indTot;
        } else {
            this.indTot = IndicadorTotal.itSomaTotalNFe;
        }
    }

    public String getxPed() { return xPed; }
    public void setxPed(String xPed) { this.xPed = xPed; }

    public int getnItemPed() { return nItemPed; }
    public void setnItemPed(int nItemPed) { this.nItemPed = nItemPed; }

    public String getnFCI() { return nFCI; }
    public void setnFCI(String nFCI) { this.nFCI = nFCI; }

    public String getnRECOPI() { return nRECOPI; }
    public void setnRECOPI(String nRECOPI) { this.nRECOPI = nRECOPI; }

    public double getpDevol() { return pDevol; }
    public void setpDevol(double pDevol) { this.pDevol = pDevol; }

    public double getvIPIDevol() { return vIPIDevol; }
    public void setvIPIDevol(double vIPIDevol) { this.vIPIDevol = vIPIDevol; }

    public double getvTotTrib() { return vTotTrib; }
    public void setvTotTrib(double vTotTrib) { this.vTotTrib = vTotTrib; }

    public String getInfAdProd() { return infAdProd; }
    public void setInfAdProd(String infAdProd) { this.infAdProd = infAdProd; }

    public IndEscala getIndEscala() { return indEscala; }
    public void setIndEscala(IndEscala indEscala)
    {
        if (indEscala != null){
            this.indEscala = indEscala;
        } else {
            this.indEscala = IndEscala.ieNenhum;
        }
    }

    public String getCNPJFab() { return CNPJFab; }
    public void setCNPJFab(String CNPJFab) { this.CNPJFab = CNPJFab; }

    public String getcBenef() { return cBenef; }
    public void setcBenef(String cBenef) { this.cBenef = cBenef; }

    public CreditoPresumidoNFe getgCred() { return gCred; }
    public void setgCred(CreditoPresumidoNFe gCred) { this.gCred = gCred; }

    public List<NVENFe> getNVE() { return NVE; }

    public List<DINFe> getDI() { return DI; }

    public List<DetExportNFe> getDetExport() { return DetExport; }

    public List<RastroNFe> getRastro() { return Rastro; }

    public List<MedicamentoNFe> getMedicamento() { return Medicamento; }

    public List<ArmaNFe> getArma() { return Arma; }

    public ImpostoDevolNFe getImpostoDevol() { return ImpostoDevol; }
    public void setImpostoDevol(ImpostoDevolNFe impostoDevol) { ImpostoDevol = impostoDevol; }

    public VeiculoNFe getVeiculo() { return Veiculo; }
    public void setVeiculo(VeiculoNFe veiculo) { Veiculo = veiculo; }

    public CombustivelNFe getCombustivel() { return Combustivel; }
    public void setCombustivel(CombustivelNFe combustivel) { Combustivel = combustivel; }

    public ICMSProdutoNFe getICMS() { return ICMS; }
    public void setICMS(ICMSProdutoNFe ICMS) { this.ICMS = ICMS; }

    public ICMSUFDESTNFe getICMSUFDEST() { return ICMSUFDEST; }
    public void setICMSUFDEST(ICMSUFDESTNFe ICMSUFDEST) { this.ICMSUFDEST = ICMSUFDEST; }

    public IPIProdutoNFe getIPI() { return IPI; }
    public void setIPI(IPIProdutoNFe IPI) { this.IPI = IPI; }

    public IIProdutoNFe getII() { return II; }
    public void setII(IIProdutoNFe II) { this.II = II; }

    public PISProdutoNFe getPIS() { return PIS; }
    public void setPIS(PISProdutoNFe PIS) { this.PIS = PIS; }

    public PISSTProdutoNFe getPISST() { return PISST; }
    public void setPISST(PISSTProdutoNFe PISST) { this.PISST = PISST; }

    public COFINSProdutoNFe getCOFINS() { return COFINS; }
    public void setCOFINS(COFINSProdutoNFe COFINS) { this.COFINS = COFINS; }

    public COFINSSTProdutoNFe getCOFINSST() { return COFINSST; }
    public void setCOFINSST(COFINSSTProdutoNFe COFINSST) { this.COFINSST = COFINSST; }

    public ISSQNNFe getISSQN() { return ISSQN; }
    public void setISSQN(ISSQNNFe ISSQN) { this.ISSQN = ISSQN; }

    public ISProdutoNFe getIS() {
        return IS;
    }

    public void setIS(ISProdutoNFe IS) {
        this.IS = IS;
    }

    public IBSCBSProdutoNFe getIBSCBS() {
        return IBSCBS;
    }

    public void setIBSCBS(IBSCBSProdutoNFe IBSCBS) {
        this.IBSCBS = IBSCBS;
    }

    public DFeReferenciado getDFeReferenciado() {
        return DFeReferenciado;
    }

    public void setDFeReferenciado(DFeReferenciado DFeReferenciado) {
        this.DFeReferenciado = DFeReferenciado;
    }

    public BigDecimal getvItem() {
        return vItem;
    }

    public void setvItem(BigDecimal vItem) {
        this.vItem = vItem;
    }

    public IndBemMovelUsado getIndBemMovelUsado() {
        return IndBemMovelUsado;
    }

    public void setIndBemMovelUsado(IndBemMovelUsado indBemMovelUsado) {
        IndBemMovelUsado = indBemMovelUsado;
    }
}