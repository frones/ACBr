package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gCBS {

    private BigDecimal pCBS;
    private BigDecimal vCBS;
    private BigDecimal pDif;
    private BigDecimal vDif;
    private BigDecimal vDevTrib;
    private BigDecimal pRedAliq;
    private BigDecimal pAliqEfet;

    public gCBS() {
    }

    public BigDecimal getpCBS() {
        return pCBS;
    }

    public void setpCBS(BigDecimal pCBS) {
        this.pCBS = pCBS;
    }

    public BigDecimal getvCBS() {
        return vCBS;
    }

    public void setvCBS(BigDecimal vCBS) {
        this.vCBS = vCBS;
    }

    public BigDecimal getpDif() {
        return pDif;
    }

    public void setpDif(BigDecimal pDif) {
        this.pDif = pDif;
    }

    public BigDecimal getvDif() {
        return vDif;
    }

    public void setvDif(BigDecimal vDif) {
        this.vDif = vDif;
    }

    public BigDecimal getvDevTrib() {
        return vDevTrib;
    }

    public void setvDevTrib(BigDecimal vDevTrib) {
        this.vDevTrib = vDevTrib;
    }

    public BigDecimal getpRedAliq() {
        return pRedAliq;
    }

    public void setpRedAliq(BigDecimal pRedAliq) {
        this.pRedAliq = pRedAliq;
    }

    public BigDecimal getpAliqEfet() {
        return pAliqEfet;
    }

    public void setpAliqEfet(BigDecimal pAliqEfet) {
        this.pAliqEfet = pAliqEfet;
    }
}
