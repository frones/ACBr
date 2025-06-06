package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBSMunTot {

    private BigDecimal vDif;
    private BigDecimal vDevTrib;
    private BigDecimal vIBSMun;

    public gIBSMunTot() {

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

    public BigDecimal getvIBSMun() {
        return vIBSMun;
    }

    public void setvIBSMun(BigDecimal vIBSMun) {
        this.vIBSMun = vIBSMun;
    }
}
