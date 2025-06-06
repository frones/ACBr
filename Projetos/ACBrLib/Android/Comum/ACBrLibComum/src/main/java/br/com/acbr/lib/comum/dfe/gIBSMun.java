package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBSMun extends gIBS{
    private BigDecimal pIBSMun;
    private BigDecimal vIBSMun;
    private BigDecimal vCBSOp;

    public gIBSMun() {
    }

    public BigDecimal getpIBSMun() {
        return pIBSMun;
    }

    public void setpIBSMun(BigDecimal pIBSMun) {
        this.pIBSMun = pIBSMun;
    }

    public BigDecimal getvIBSMun() {
        return vIBSMun;
    }

    public void setvIBSMun(BigDecimal vIBSMun) {
        this.vIBSMun = vIBSMun;
    }

    public BigDecimal getvCBSOp() {
        return vCBSOp;
    }

    public void setvCBSOp(BigDecimal vCBSOp) {
        this.vCBSOp = vCBSOp;
    }
}
