package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gIBSCBS {

    public gIBSCBS(){
        gIBSUF = new gIBSUF();
        gIBSMun = new gIBSMun();
        gCBS = new gCBS();
        gTribRegular = new gTribRegular();
        gIBSCredPres = new gCredPres();
        gCBSCredPres = new gCredPres();
    }

    private BigDecimal vBC;
    private gIBSUF gIBSUF;
    private gIBSMun gIBSMun;
    private gCBS gCBS;
    private gTribRegular gTribRegular;
    private gCredPres gIBSCredPres;
    private gCredPres gCBSCredPres;

    public BigDecimal getvBC() {
        return vBC;
    }

    public void setvBC(BigDecimal vBC) {
        this.vBC = vBC;
    }

    public gIBSUF getgIBSUF() {
        return gIBSUF;
    }

    public void setgIBSUF(gIBSUF gIBSUF) {
        this.gIBSUF = gIBSUF;
    }

    public gIBSMun getgIBSMun() {
        return gIBSMun;
    }

    public void setgIBSMun(gIBSMun gIBSMun) {
        this.gIBSMun = gIBSMun;
    }

    public br.com.acbr.lib.comum.dfe.gCBS getgCBS() {
        return gCBS;
    }

    public void setgCBS(br.com.acbr.lib.comum.dfe.gCBS gCBS) {
        this.gCBS = gCBS;
    }

    public gTribRegular getgTribRegular() {
        return gTribRegular;
    }

    public void setgTribRegular(gTribRegular gTribRegular) {
        this.gTribRegular = gTribRegular;
    }

    public gCredPres getgIBSCredPres() {
        return gIBSCredPres;
    }

    public void setgIBSCredPres(gCredPres gIBSCredPres) {
        this.gIBSCredPres = gIBSCredPres;
    }

    public gCredPres getgCBSCredPres() {
        return gCBSCredPres;
    }

    public void setgCBSCredPres(gCredPres gCBSCredPres) {
        this.gCBSCredPres = gCBSCredPres;
    }
}
