package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gCBSTot {

    private BigDecimal vDif;
    private BigDecimal vDevTrib;
    private BigDecimal vCBS;
    private BigDecimal vCredPres;
    private BigDecimal vCredPresCondSus;

    public gCBSTot() {

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

    public BigDecimal getvCBS() {
        return vCBS;
    }

    public void setvCBS(BigDecimal vCBS) {
        this.vCBS = vCBS;
    }

    public BigDecimal getvCredPres() {
        return vCredPres;
    }

    public void setvCredPres(BigDecimal vCredPres) {
        this.vCredPres = vCredPres;
    }

    public BigDecimal getvCredPresCondSus() {
        return vCredPresCondSus;
    }

    public void setvCredPresCondSus(BigDecimal vCredPresCondSus) {
        this.vCredPresCondSus = vCredPresCondSus;
    }
}
