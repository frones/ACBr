package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.util.ArrayList;

import br.com.acbr.lib.comum.dfe.CSOSNIcms;
import br.com.acbr.lib.comum.dfe.CSTIcms;
import br.com.acbr.lib.comum.dfe.OrigemMercadoria;

public class ICMSProdutoNFe {
    private OrigemMercadoria orig;
    private CSTIcms CST;
    private CSOSNIcms CSOSN;
    private DeterminacaoBaseIcms modBC;
    private BigDecimal pRedBC;
    private BigDecimal vBC;
    private BigDecimal pICMS;
    private BigDecimal vICMS;
    private DeterminacaoBaseIcmsST modBCST;
    private BigDecimal pMVAST;
    private BigDecimal pRedBCST;
    private BigDecimal vBCST;
    private BigDecimal pICMSST;
    private BigDecimal vICMSST;
    private BigDecimal UFST;
    private BigDecimal pBCOp;
    private BigDecimal vBCSTRet;
    private BigDecimal vICMSSTRet;
    private MotivoDesoneracaoICMS motDesICMS;
    private BigDecimal vICMSDeson;
    private int indDeduzDeson;
    private BigDecimal pCredSN;
    private BigDecimal vCredICMSSN;
    private BigDecimal vBCSTDest;
    private BigDecimal vICMSSTDest;
    private BigDecimal vICMSOp;
    private BigDecimal pDif;
    private BigDecimal vICMSDif;
    private BigDecimal pST;
    private BigDecimal vBCFCP;
    private BigDecimal pFCP;
    private BigDecimal vFCP;
    private BigDecimal vBCFCPST;
    private BigDecimal pFCPST;
    private BigDecimal vFCPST;
    private BigDecimal vBCFCPSTRet;
    private BigDecimal pFCPSTRet;
    private BigDecimal vFCPSTRet;
    private BigDecimal pRedBCEfet;
    private BigDecimal vBCEfet;
    private BigDecimal pICMSEfet;
    private BigDecimal vICMSEfet;
    private BigDecimal vICMSSubstituto;
    private BigDecimal adRemICMS;
    private BigDecimal vICMSMono;
    private BigDecimal adRemICMSReten;
    private BigDecimal vICMSMonoReten;
    private BigDecimal vICMSMonoDif;
    private BigDecimal adRemICMSRet;
    private BigDecimal vICMSMonoRet;
    private BigDecimal qBCMono;
    private BigDecimal qBCMonoReten;
    private BigDecimal pRedAdRem;
    private MotRedAdRem motRedAdRem;
    private BigDecimal qBCMonoRet;
    private BigDecimal vICMSMonoOp;
    private BigDecimal pFCPDif;
    private BigDecimal vFCPDif;
    private BigDecimal vFCPEfet;

    public ICMSProdutoNFe() {
    }

    public OrigemMercadoria getOrig() {
        return orig;
    }

    public void setOrig(OrigemMercadoria orig) {
        if(orig != null){
            this.orig = orig;
        } else {
            this.orig = OrigemMercadoria.oeNacional;
        }
    }

    public CSTIcms getCST() {
        return CST;
    }

    public void setCST(CSTIcms CST) {
        if(CST != null){
            this.CST = CST;
        } else {
            this.CST = CSTIcms.cstVazio;
        }
    }

    public CSOSNIcms getCSOSN() {
        return CSOSN;
    }

    public void setCSOSN(CSOSNIcms CSOSN) {
        if(CSOSN != null){
            this.CSOSN = CSOSN;
        } else {
            this.CSOSN = CSOSNIcms.csosnVazio;
        }
    }

    public DeterminacaoBaseIcms getModBC() {
        return modBC;
    }

    public void setModBC(DeterminacaoBaseIcms modBC) {
        if(modBC != null){
            this.modBC = modBC;
        } else {
            this.modBC = DeterminacaoBaseIcms.dbiNenhum;
        }
    }

    public BigDecimal getpRedBC() {
        return pRedBC;
    }

    public void setpRedBC(BigDecimal pRedBC) {
        this.pRedBC = pRedBC;
    }

    public BigDecimal getvBC() {
        return vBC;
    }

    public void setvBC(BigDecimal vBC) {
        this.vBC = vBC;
    }

    public BigDecimal getpICMS() {
        return pICMS;
    }

    public void setpICMS(BigDecimal pICMS) {
        this.pICMS = pICMS;
    }

    public BigDecimal getvICMS() {
        return vICMS;
    }

    public void setvICMS(BigDecimal vICMS) {
        this.vICMS = vICMS;
    }

    public DeterminacaoBaseIcmsST getModBCST() {
        return modBCST;
    }

    public void setModBCST(DeterminacaoBaseIcmsST modBCST) {
        if(modBCST != null){
            this.modBCST = modBCST;
        } else {
            this.modBCST = DeterminacaoBaseIcmsST.dbisPrecoTabelado;
        }
    }

    public BigDecimal getpMVAST() {
        return pMVAST;
    }

    public void setpMVAST(BigDecimal pMVAST) {
        this.pMVAST = pMVAST;
    }

    public BigDecimal getpRedBCST() {
        return pRedBCST;
    }

    public void setpRedBCST(BigDecimal pRedBCST) {
        this.pRedBCST = pRedBCST;
    }

    public BigDecimal getvBCST() {
        return vBCST;
    }

    public void setvBCST(BigDecimal vBCST) {
        this.vBCST = vBCST;
    }

    public BigDecimal getpICMSST() {
        return pICMSST;
    }

    public void setpICMSST(BigDecimal pICMSST) {
        this.pICMSST = pICMSST;
    }

    public BigDecimal getvICMSST() {
        return vICMSST;
    }

    public void setvICMSST(BigDecimal vICMSST) {
        this.vICMSST = vICMSST;
    }

    public BigDecimal getUFST() {
        return UFST;
    }

    public void setUFST(BigDecimal UFST) {
        this.UFST = UFST;
    }

    public BigDecimal getpBCOp() {
        return pBCOp;
    }

    public void setpBCOp(BigDecimal pBCOp) {
        this.pBCOp = pBCOp;
    }

    public BigDecimal getvBCSTRet() {
        return vBCSTRet;
    }

    public void setvBCSTRet(BigDecimal vBCSTRet) {
        this.vBCSTRet = vBCSTRet;
    }

    public BigDecimal getvICMSSTRet() {
        return vICMSSTRet;
    }

    public void setvICMSSTRet(BigDecimal vICMSSTRet) {
        this.vICMSSTRet = vICMSSTRet;
    }

    public MotivoDesoneracaoICMS getMotDesICMS() {
        return motDesICMS;
    }

    public void setMotDesICMS(MotivoDesoneracaoICMS motDesICMS) {
        if(motDesICMS != null){
            this.motDesICMS = motDesICMS;
        } else {
            this.motDesICMS = MotivoDesoneracaoICMS.mdiOutros;
        }
    }

    public BigDecimal getvICMSDeson() {
        return vICMSDeson;
    }

    public void setvICMSDeson(BigDecimal vICMSDeson) {
        this.vICMSDeson = vICMSDeson;
    }

    public int getIndDeduzDeson() {
        return indDeduzDeson;
    }

    public void setIndDeduzDeson(int indDeduzDeson) {
        this.indDeduzDeson = indDeduzDeson;
    }

    public BigDecimal getpCredSN() {
        return pCredSN;
    }

    public void setpCredSN(BigDecimal pCredSN) {
        this.pCredSN = pCredSN;
    }

    public BigDecimal getvCredICMSSN() {
        return vCredICMSSN;
    }

    public void setvCredICMSSN(BigDecimal vCredICMSSN) {
        this.vCredICMSSN = vCredICMSSN;
    }

    public BigDecimal getvBCSTDest() {
        return vBCSTDest;
    }

    public void setvBCSTDest(BigDecimal vBCSTDest) {
        this.vBCSTDest = vBCSTDest;
    }

    public BigDecimal getvICMSSTDest() {
        return vICMSSTDest;
    }

    public void setvICMSSTDest(BigDecimal vICMSSTDest) {
        this.vICMSSTDest = vICMSSTDest;
    }

    public BigDecimal getvICMSOp() {
        return vICMSOp;
    }

    public void setvICMSOp(BigDecimal vICMSOp) {
        this.vICMSOp = vICMSOp;
    }

    public BigDecimal getpDif() {
        return pDif;
    }

    public void setpDif(BigDecimal pDif) {
        this.pDif = pDif;
    }

    public BigDecimal getvICMSDif() {
        return vICMSDif;
    }

    public void setvICMSDif(BigDecimal vICMSDif) {
        this.vICMSDif = vICMSDif;
    }

    public BigDecimal getpST() {
        return pST;
    }

    public void setpST(BigDecimal pST) {
        this.pST = pST;
    }

    public BigDecimal getvBCFCP() {
        return vBCFCP;
    }

    public void setvBCFCP(BigDecimal vBCFCP) {
        this.vBCFCP = vBCFCP;
    }

    public BigDecimal getpFCP() {
        return pFCP;
    }

    public void setpFCP(BigDecimal pFCP) {
        this.pFCP = pFCP;
    }

    public BigDecimal getvFCP() {
        return vFCP;
    }

    public void setvFCP(BigDecimal vFCP) {
        this.vFCP = vFCP;
    }

    public BigDecimal getvBCFCPST() {
        return vBCFCPST;
    }

    public void setvBCFCPST(BigDecimal vBCFCPST) {
        this.vBCFCPST = vBCFCPST;
    }

    public BigDecimal getpFCPST() {
        return pFCPST;
    }

    public void setpFCPST(BigDecimal pFCPST) {
        this.pFCPST = pFCPST;
    }

    public BigDecimal getvFCPST() {
        return vFCPST;
    }

    public void setvFCPST(BigDecimal vFCPST) {
        this.vFCPST = vFCPST;
    }

    public BigDecimal getvBCFCPSTRet() {
        return vBCFCPSTRet;
    }

    public void setvBCFCPSTRet(BigDecimal vBCFCPSTRet) {
        this.vBCFCPSTRet = vBCFCPSTRet;
    }

    public BigDecimal getpFCPSTRet() {
        return pFCPSTRet;
    }

    public void setpFCPSTRet(BigDecimal pFCPSTRet) {
        this.pFCPSTRet = pFCPSTRet;
    }

    public BigDecimal getvFCPSTRet() {
        return vFCPSTRet;
    }

    public void setvFCPSTRet(BigDecimal vFCPSTRet) {
        this.vFCPSTRet = vFCPSTRet;
    }

    public BigDecimal getpRedBCEfet() {
        return pRedBCEfet;
    }

    public void setpRedBCEfet(BigDecimal pRedBCEfet) {
        this.pRedBCEfet = pRedBCEfet;
    }

    public BigDecimal getvBCEfet() {
        return vBCEfet;
    }

    public void setvBCEfet(BigDecimal vBCEfet) {
        this.vBCEfet = vBCEfet;
    }

    public BigDecimal getpICMSEfet() {
        return pICMSEfet;
    }

    public void setpICMSEfet(BigDecimal pICMSEfet) {
        this.pICMSEfet = pICMSEfet;
    }

    public BigDecimal getvICMSEfet() {
        return vICMSEfet;
    }

    public void setvICMSEfet(BigDecimal vICMSEfet) {
        this.vICMSEfet = vICMSEfet;
    }

    public BigDecimal getvICMSSubstituto() {
        return vICMSSubstituto;
    }

    public void setvICMSSubstituto(BigDecimal vICMSSubstituto) {
        this.vICMSSubstituto = vICMSSubstituto;
    }

    public BigDecimal getAdRemICMS() {
        return adRemICMS;
    }

    public void setAdRemICMS(BigDecimal adRemICMS) {
        this.adRemICMS = adRemICMS;
    }

    public BigDecimal getvICMSMono() {
        return vICMSMono;
    }

    public void setvICMSMono(BigDecimal vICMSMono) {
        this.vICMSMono = vICMSMono;
    }

    public BigDecimal getAdRemICMSReten() {
        return adRemICMSReten;
    }

    public void setAdRemICMSReten(BigDecimal adRemICMSReten) {
        this.adRemICMSReten = adRemICMSReten;
    }

    public BigDecimal getvICMSMonoReten() {
        return vICMSMonoReten;
    }

    public void setvICMSMonoReten(BigDecimal vICMSMonoReten) {
        this.vICMSMonoReten = vICMSMonoReten;
    }

    public BigDecimal getvICMSMonoDif() {
        return vICMSMonoDif;
    }

    public void setvICMSMonoDif(BigDecimal vICMSMonoDif) {
        this.vICMSMonoDif = vICMSMonoDif;
    }

    public BigDecimal getAdRemICMSRet() {
        return adRemICMSRet;
    }

    public void setAdRemICMSRet(BigDecimal adRemICMSRet) {
        this.adRemICMSRet = adRemICMSRet;
    }

    public BigDecimal getvICMSMonoRet() {
        return vICMSMonoRet;
    }

    public void setvICMSMonoRet(BigDecimal vICMSMonoRet) {
        this.vICMSMonoRet = vICMSMonoRet;
    }

    public BigDecimal getQBCMono() {
        return qBCMono;
    }

    public void setQBCMono(BigDecimal qBCMono) {
        this.qBCMono = qBCMono;
    }

    public BigDecimal getQBCMonoReten() {
        return qBCMonoReten;
    }

    public void setQBCMonoReten(BigDecimal qBCMonoReten) {
        this.qBCMonoReten = qBCMonoReten;
    }

    public BigDecimal getpRedAdRem() {
        return pRedAdRem;
    }

    public void setpRedAdRem(BigDecimal pRedAdRem) {
        this.pRedAdRem = pRedAdRem;
    }

    public MotRedAdRem getMotRedAdRem() {
        return motRedAdRem;
    }

    public void setMotRedAdRem(MotRedAdRem motRedAdRem) {
        if(motRedAdRem != null){
            this.motRedAdRem = motRedAdRem;
        } else {
            this.motRedAdRem = MotRedAdRem.motOutros;
        }
    }

    public BigDecimal getqBCMonoRet() {
        return qBCMonoRet;
    }

    public void setqBCMonoRet(BigDecimal qBCMonoRet) {
        this.qBCMonoRet = qBCMonoRet;
    }

    public BigDecimal getvICMSMonoOp() {
        return vICMSMonoOp;
    }

    public void setvICMSMonoOp(BigDecimal vICMSMonoOp) {
        this.vICMSMonoOp = vICMSMonoOp;
    }

    public BigDecimal getpFCPDif() {
        return pFCPDif;
    }

    public void setpFCPDif(BigDecimal pFCPDif) {
        this.pFCPDif = pFCPDif;
    }

    public BigDecimal getvFCPDif() {
        return vFCPDif;
    }

    public void setvFCPDif(BigDecimal vFCPDif) {
        this.vFCPDif = vFCPDif;
    }

    public BigDecimal getvFCPEfet() {
        return vFCPEfet;
    }

    public void setvFCPEfet(BigDecimal vFCPEfet) {
        this.vFCPEfet = vFCPEfet;
    }
}
