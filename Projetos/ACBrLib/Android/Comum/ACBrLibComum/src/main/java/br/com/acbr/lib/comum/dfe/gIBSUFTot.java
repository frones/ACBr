package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBSUFTot {

    private BigDecimal vDif;
    private BigDecimal vDevTrib;
    private BigDecimal vIBSUF;

    public gIBSUFTot() {
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

    public BigDecimal getvIBSUF() {
        return vIBSUF;
    }

    public void setvIBSUF(BigDecimal vIBSUF) {
        this.vIBSUF = vIBSUF;
    }
}
