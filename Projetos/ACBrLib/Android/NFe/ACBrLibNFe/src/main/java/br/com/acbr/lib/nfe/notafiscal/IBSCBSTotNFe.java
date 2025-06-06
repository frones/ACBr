package br.com.acbr.lib.nfe.notafiscal;

import br.com.acbr.lib.comum.dfe.gCBSTot;
import br.com.acbr.lib.comum.dfe.gIBSTot;
import br.com.acbr.lib.nfe.gIBSCBSMonoTot;

public class IBSCBSTotNFe {

    public IBSCBSTotNFe(){
        gIBS = new gIBSTot();
        gCBS = new gCBSTot();
        gMono = new gIBSCBSMonoTot();
    }

    private double vBCIBSCBS;
    private gIBSTot gIBS;
    private gCBSTot gCBS;
    private gIBSCBSMonoTot gMono;

    public double getvBCIBSCBS() {
        return vBCIBSCBS;
    }

    public void setvBCIBSCBS(double vBCIBSCBS) {
        this.vBCIBSCBS = vBCIBSCBS;
    }

    public gIBSTot getgIBS() {
        return gIBS;
    }

    public void setgIBS(gIBSTot gIBS) {
        this.gIBS = gIBS;
    }

    public gCBSTot getgCBS() {
        return gCBS;
    }

    public void setgCBS(gCBSTot gCBS) {
        this.gCBS = gCBS;
    }

    public gIBSCBSMonoTot getgMono() {
        return gMono;
    }

    public void setgMono(gIBSCBSMonoTot gMono) {
        this.gMono = gMono;
    }
}
