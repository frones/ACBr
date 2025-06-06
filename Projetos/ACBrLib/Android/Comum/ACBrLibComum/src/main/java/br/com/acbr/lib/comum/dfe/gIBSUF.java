package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBSUF extends gIBS{

    private BigDecimal pIBSUF;
    private BigDecimal vIBSUF;

    public gIBSUF(){
    }

    public BigDecimal getpIBSUF() {
        return pIBSUF;
    }

    public void setpIBSUF(BigDecimal pIBSUF) {
        this.pIBSUF = pIBSUF;
    }

    public BigDecimal getvIBSUF() {
        return vIBSUF;
    }

    public void setvIBSUF(BigDecimal vIBSUF) {
        this.vIBSUF = vIBSUF;
    }
}
