package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBSTot {

    public gIBSTot() {
        gIBSMun = new gIBSMunTot();
        gIBSUF = new gIBSUFTot();
    }

    private BigDecimal vIBS;
    private BigDecimal vCredPres;
    private BigDecimal vCredPresCondSus;
    private gIBSUFTot gIBSUF;
    private gIBSMunTot gIBSMun;

    public BigDecimal getvIBS() {
        return vIBS;
    }

    public void setvIBS(BigDecimal vIBS) {
        this.vIBS = vIBS;
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

    public gIBSUFTot getgIBSUF() {
        return gIBSUF;
    }

    public void setgIBSUF(gIBSUFTot gIBSUF) {
        this.gIBSUF = gIBSUF;
    }

    public gIBSMunTot getgIBSMun() {
        return gIBSMun;
    }

    public void setgIBSMun(gIBSMunTot gIBSMun) {
        this.gIBSMun = gIBSMun;
    }
}
