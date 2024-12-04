package br.com.acbr.lib.nfe.notafiscal;

import java.text.SimpleDateFormat;
import java.util.Date;

public class RastroNFe {
    private String nLote;
    private double qLote;
    private String dFab;
    private String dVal;
    private String cAgreg;

    public RastroNFe() {
    }

    public String getnLote() {
        return nLote;
    }

    public void setnLote(String nLote) {
        this.nLote = nLote;
    }

    public double getqLote() {
        return qLote;
    }

    public void setqLote(double qLote) {
        this.qLote = qLote;
    }

    public String getdFab() {
        return this.dFab;
    }

    public void setdFab(Date dFab) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dFab = sdf.format(dFab);
    }

    public String getdVal() {
        return dVal;
    }

    public void setdVal(Date dVal) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dVal = sdf.format(dVal);
    }

    public String getcAgreg() {
        return cAgreg;
    }

    public void setcAgreg(String cAgreg) {
        this.cAgreg = cAgreg;
    }
}
