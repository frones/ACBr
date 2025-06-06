package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBS {

    private BigDecimal pDif;
    private BigDecimal vDif;
    private BigDecimal vDevTrib;
    private BigDecimal pRedAliq;
    private BigDecimal pAliqEfet;

    public gIBS() {
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
