package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gCredPres {

    private TcCredPres cCredPres;
    private BigDecimal pCredPres;
    private BigDecimal vCredPres;
    private BigDecimal vCredPresCondSus;

    public gCredPres() {
    }

    public TcCredPres getcCredPres() {
        return cCredPres;
    }

    public void setcCredPres(TcCredPres cCredPres) {
        this.cCredPres = cCredPres;
    }

    public BigDecimal getpCredPres() {
        return pCredPres;
    }

    public void setpCredPres(BigDecimal pCredPres) {
        this.pCredPres = pCredPres;
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
