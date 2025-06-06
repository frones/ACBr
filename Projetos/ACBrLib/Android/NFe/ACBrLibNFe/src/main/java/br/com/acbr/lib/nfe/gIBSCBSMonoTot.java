package br.com.acbr.lib.nfe;

import java.math.BigDecimal;

public class gIBSCBSMonoTot {

    private BigDecimal vIBSMono;
    private BigDecimal vCBSMono;
    private BigDecimal vIBSMonoReten;
    private BigDecimal vCBSMonoReten;
    private BigDecimal vIBSMonoRet;
    private BigDecimal vCBSMonoRet;

    public gIBSCBSMonoTot(){

    }

    public BigDecimal getvIBSMono() {
        return vIBSMono;
    }

    public void setvIBSMono(BigDecimal vIBSMono) {
        this.vIBSMono = vIBSMono;
    }

    public BigDecimal getvCBSMono() {
        return vCBSMono;
    }

    public void setvCBSMono(BigDecimal vCBSMono) {
        this.vCBSMono = vCBSMono;
    }

    public BigDecimal getvIBSMonoReten() {
        return vIBSMonoReten;
    }

    public void setvIBSMonoReten(BigDecimal vIBSMonoReten) {
        this.vIBSMonoReten = vIBSMonoReten;
    }

    public BigDecimal getvCBSMonoReten() {
        return vCBSMonoReten;
    }

    public void setvCBSMonoReten(BigDecimal vCBSMonoReten) {
        this.vCBSMonoReten = vCBSMonoReten;
    }

    public BigDecimal getvIBSMonoRet() {
        return vIBSMonoRet;
    }

    public void setvIBSMonoRet(BigDecimal vIBSMonoRet) {
        this.vIBSMonoRet = vIBSMonoRet;
    }

    public BigDecimal getvCBSMonoRet() {
        return vCBSMonoRet;
    }

    public void setvCBSMonoRet(BigDecimal vCBSMonoRet) {
        this.vCBSMonoRet = vCBSMonoRet;
    }
}
