package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;

public class RetTribNFe {
    private BigDecimal vRetPIS;
    private BigDecimal vRetCOFINS;
    private BigDecimal vRetCSLL;
    private BigDecimal vBCIRRF;
    private BigDecimal vIRRF;
    private BigDecimal vBCRetPrev;
    private BigDecimal vRetPrev;

    public RetTribNFe() {}

    public BigDecimal getvRetPIS() {
        return vRetPIS;
    }

    public void setvRetPIS(BigDecimal vRetPIS) {
        this.vRetPIS = vRetPIS;
    }

    public BigDecimal getvRetCOFINS() {
        return vRetCOFINS;
    }

    public void setvRetCOFINS(BigDecimal vRetCOFINS) {
        this.vRetCOFINS = vRetCOFINS;
    }

    public BigDecimal getvRetCSLL() {
        return vRetCSLL;
    }

    public void setvRetCSLL(BigDecimal vRetCSLL) {
        this.vRetCSLL = vRetCSLL;
    }

    public BigDecimal getvBCIRRF() {
        return vBCIRRF;
    }

    public void setvBCIRRF(BigDecimal vBCIRRF) {
        this.vBCIRRF = vBCIRRF;
    }

    public BigDecimal getvIRRF() {
        return vIRRF;
    }

    public void setvIRRF(BigDecimal vIRRF) {
        this.vIRRF = vIRRF;
    }

    public BigDecimal getvBCRetPrev() {
        return vBCRetPrev;
    }

    public void setvBCRetPrev(BigDecimal vBCRetPrev) {
        this.vBCRetPrev = vBCRetPrev;
    }

    public BigDecimal getvRetPrev() {
        return vRetPrev;
    }

    public void setvRetPrev(BigDecimal vRetPrev) {
        this.vRetPrev = vRetPrev;
    }
}
