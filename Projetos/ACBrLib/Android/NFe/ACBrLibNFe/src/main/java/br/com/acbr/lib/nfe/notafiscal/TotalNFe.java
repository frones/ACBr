package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;

public class TotalNFe {

    public TotalNFe() {
        ISTot = new ISTotNFe();
        IBSCBSTot = new IBSCBSTotNFe();
    }

    private BigDecimal vBC;
    private BigDecimal vICMS;
    private BigDecimal vICMSDeson;
    private BigDecimal vBCST;
    private BigDecimal vST;
    private BigDecimal vProd;
    private BigDecimal vFrete;
    private BigDecimal vSeg;
    private BigDecimal vDesc;
    private BigDecimal vII;
    private BigDecimal vIPI;
    private BigDecimal vPIS;
    private BigDecimal vCOFINS;
    private BigDecimal vOutro;
    private BigDecimal vNF;
    private BigDecimal vTotTrib;
    private BigDecimal vFCP;
    private BigDecimal vFCPST;
    private BigDecimal vFCPSTRet;
    private BigDecimal vIPIDevol;
    private BigDecimal vFCPUFDest;
    private BigDecimal vICMSUFDest;
    private BigDecimal vICMSUFRemet;
    private BigDecimal qBCMono;
    private BigDecimal vICMSMono;
    private BigDecimal qBCMonoReten;
    private BigDecimal vICMSMonoReten;
    private BigDecimal qBCMonoRet;
    private BigDecimal vICMSMonoRet;
    private ISTotNFe ISTot;
    private IBSCBSTotNFe IBSCBSTot;
    private BigDecimal vNFTot;

    public BigDecimal getVBC() {
        return vBC;
    }

    public void setVBC(BigDecimal vBC) {
        this.vBC = vBC;
    }

    public BigDecimal getVICMS() {
        return vICMS;
    }

    public void setVICMS(BigDecimal vICMS) {
        this.vICMS = vICMS;
    }

    public BigDecimal getVICMSDeson() {
        return vICMSDeson;
    }

    public void setVICMSDeson(BigDecimal vICMSDeson) {
        this.vICMSDeson = vICMSDeson;
    }

    public BigDecimal getVBCST() {
        return vBCST;
    }

    public void setVBCST(BigDecimal vBCST) {
        this.vBCST = vBCST;
    }

    public BigDecimal getVST() {
        return vST;
    }

    public void setVST(BigDecimal vST) {
        this.vST = vST;
    }

    public BigDecimal getVProd() {
        return vProd;
    }

    public void setVProd(BigDecimal vProd) {
        this.vProd = vProd;
    }

    public BigDecimal getVFrete() {
        return vFrete;
    }

    public void setVFrete(BigDecimal vFrete) {
        this.vFrete = vFrete;
    }

    public BigDecimal getVSeg() {
        return vSeg;
    }

    public void setVSeg(BigDecimal vSeg) {
        this.vSeg = vSeg;
    }

    public BigDecimal getVDesc() {
        return vDesc;
    }

    public void setVDesc(BigDecimal vDesc) {
        this.vDesc = vDesc;
    }

    public BigDecimal getVII() {
        return vII;
    }

    public void setVII(BigDecimal vII) {
        this.vII = vII;
    }

    public BigDecimal getVIPI() {
        return vIPI;
    }

    public void setVIPI(BigDecimal vIPI) {
        this.vIPI = vIPI;
    }

    public BigDecimal getVPIS() {
        return vPIS;
    }

    public void setVPIS(BigDecimal vPIS) {
        this.vPIS = vPIS;
    }

    public BigDecimal getVCOFINS() {
        return vCOFINS;
    }

    public void setVCOFINS(BigDecimal vCOFINS) {
        this.vCOFINS = vCOFINS;
    }

    public BigDecimal getVOutro() {
        return vOutro;
    }

    public void setVOutro(BigDecimal vOutro) {
        this.vOutro = vOutro;
    }

    public BigDecimal getVNF() {
        return vNF;
    }

    public void setVNF(BigDecimal vNF) {
        this.vNF = vNF;
    }

    public BigDecimal getVTotTrib() {
        return vTotTrib;
    }

    public void setVTotTrib(BigDecimal vTotTrib) {
        this.vTotTrib = vTotTrib;
    }

    public BigDecimal getVFCP() {
        return vFCP;
    }

    public void setVFCP(BigDecimal vFCP) {
        this.vFCP = vFCP;
    }

    public BigDecimal getVFCPST() {
        return vFCPST;
    }

    public void setVFCPST(BigDecimal vFCPST) {
        this.vFCPST = vFCPST;
    }

    public BigDecimal getVFCPSTRet() {
        return vFCPSTRet;
    }

    public void setVFCPSTRet(BigDecimal vFCPSTRet) {
        this.vFCPSTRet = vFCPSTRet;
    }

    public BigDecimal getVIPIDevol() {
        return vIPIDevol;
    }

    public void setVIPIDevol(BigDecimal vIPIDevol) {
        this.vIPIDevol = vIPIDevol;
    }

    public BigDecimal getVFCPUFDest() {
        return vFCPUFDest;
    }

    public void setVFCPUFDest(BigDecimal vFCPUFDest) {
        this.vFCPUFDest = vFCPUFDest;
    }

    public BigDecimal getVICMSUFDest() {
        return vICMSUFDest;
    }

    public void setVICMSUFDest(BigDecimal vICMSUFDest) {
        this.vICMSUFDest = vICMSUFDest;
    }

    public BigDecimal getVICMSUFRemet() {
        return vICMSUFRemet;
    }

    public void setVICMSUFRemet(BigDecimal vICMSUFRemet) {
        this.vICMSUFRemet = vICMSUFRemet;
    }

    public BigDecimal getQBCMono() {
        return qBCMono;
    }

    public void setQBCMono(BigDecimal qBCMono) {
        this.qBCMono = qBCMono;
    }

    public BigDecimal getVICMSMono() {
        return vICMSMono;
    }

    public void setVICMSMono(BigDecimal vICMSMono) {
        this.vICMSMono = vICMSMono;
    }

    public BigDecimal getQBCMonoReten() {
        return qBCMonoReten;
    }

    public void setQBCMonoReten(BigDecimal qBCMonoReten) {
        this.qBCMonoReten = qBCMonoReten;
    }

    public BigDecimal getVICMSMonoReten() {
        return vICMSMonoReten;
    }

    public void setVICMSMonoReten(BigDecimal vICMSMonoReten) {
        this.vICMSMonoReten = vICMSMonoReten;
    }

    public BigDecimal getQBCMonoRet() {
        return qBCMonoRet;
    }

    public void setQBCMonoRet(BigDecimal qBCMonoRet) {
        this.qBCMonoRet = qBCMonoRet;
    }

    public BigDecimal getVICMSMonoRet() {
        return vICMSMonoRet;
    }

    public void setVICMSMonoRet(BigDecimal vICMSMonoRet) {
        this.vICMSMonoRet = vICMSMonoRet;
    }

    public ISTotNFe getISTot() {
        return ISTot;
    }

    public void setISTot(ISTotNFe ISTot) {
        this.ISTot = ISTot;
    }

    public IBSCBSTotNFe getIBSCBSTot() {
        return IBSCBSTot;
    }

    public void setIBSCBSTot(IBSCBSTotNFe IBSCBSTot) {
        this.IBSCBSTot = IBSCBSTot;
    }

    public BigDecimal getvNFTot() {
        return vNFTot;
    }

    public void setvNFTot(BigDecimal vNFTot) {
        this.vNFTot = vNFTot;
    }
}
