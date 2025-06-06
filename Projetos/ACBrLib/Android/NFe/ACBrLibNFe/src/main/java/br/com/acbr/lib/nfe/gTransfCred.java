package br.com.acbr.lib.nfe;

import java.math.BigDecimal;

public class gTransfCred {

    private BigDecimal vIBS;
    private BigDecimal vCBS;

    public gTransfCred(){

    }

    public BigDecimal getvIBS() {
        return vIBS;
    }

    public void setvIBS(BigDecimal vIBS) {
        this.vIBS = vIBS;
    }

    public BigDecimal getvCBS() {
        return vCBS;
    }

    public void setvCBS(BigDecimal vCBS) {
        this.vCBS = vCBS;
    }
}
