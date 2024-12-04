package br.com.acbr.lib.nfe.notafiscal;

import java.util.ArrayList;

public class OrigCombNFe {
    private IndImport indImport;
    private int cUFOrig;
    private double pOrig;

    public OrigCombNFe() {
    }

    public IndImport getIndImport() {
        return indImport;
    }

    public void setIndImport(IndImport indImport) {
        if(indImport != null){
            this.indImport = indImport;
        } else {
            this.indImport = indImport.Nacional;
        }
    }

    public int getCUFOrig() {
        return cUFOrig;
    }

    public void setCUFOrig(int cUFOrig) {
        this.cUFOrig = cUFOrig;
    }

    public double getPOrig() {
        return pOrig;
    }

    public void setPOrig(double pOrig) {
        this.pOrig = pOrig;
    }
}
