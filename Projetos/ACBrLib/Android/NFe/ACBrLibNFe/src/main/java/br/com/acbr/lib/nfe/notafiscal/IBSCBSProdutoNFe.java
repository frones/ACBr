package br.com.acbr.lib.nfe.notafiscal;

import br.com.acbr.lib.comum.dfe.CSTIBSCBS;
import br.com.acbr.lib.comum.dfe.cClassTribIBSCBS;
import br.com.acbr.lib.comum.dfe.gIBSCBS;
import br.com.acbr.lib.nfe.gIBSCBSMono;
import br.com.acbr.lib.nfe.gTransfCred;

public class IBSCBSProdutoNFe {

    public IBSCBSProdutoNFe(){
        gIBSCBS = new gIBSCBS();
        gIBSCBSMono = new gIBSCBSMono();
        gTransfCred = new gTransfCred();
    }

    private CSTIBSCBS CST;
    private cClassTribIBSCBS cClassTrib;
    private gIBSCBS gIBSCBS;
    private gIBSCBSMono gIBSCBSMono;
    private gTransfCred gTransfCred;

    public CSTIBSCBS getCST() {
        return CST;
    }

    public void setCST(CSTIBSCBS CST) {
        this.CST = CST;
    }

    public cClassTribIBSCBS getcClassTrib() {
        return cClassTrib;
    }

    public void setcClassTrib(cClassTribIBSCBS cClassTrib) {
        this.cClassTrib = cClassTrib;
    }

    public gIBSCBS getgIBSCBS() {
        return gIBSCBS;
    }

    public void setgIBSCBS(gIBSCBS gIBSCBS) {
        this.gIBSCBS = gIBSCBS;
    }

    public gIBSCBSMono getgIBSCBSMono() {
        return gIBSCBSMono;
    }

    public void setgIBSCBSMono(gIBSCBSMono gIBSCBSMono) {
        this.gIBSCBSMono = gIBSCBSMono;
    }

    public gTransfCred getgTransfCred() {
        return gTransfCred;
    }

    public void setgTransfCred(gTransfCred gTransfCred) {
        this.gTransfCred = gTransfCred;
    }
}
