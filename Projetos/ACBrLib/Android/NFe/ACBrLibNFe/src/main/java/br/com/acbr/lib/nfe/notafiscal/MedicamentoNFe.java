package br.com.acbr.lib.nfe.notafiscal;

import java.text.SimpleDateFormat;
import java.util.Date;

public class MedicamentoNFe {
    private String cProdANVISA;
    private double vPMC;
    private String nLote;
    private double qLote;
    private String dFab;
    private String dVal;
    private String xMotivoIsencao;

    public MedicamentoNFe() {
    }

    public String getcProdANVISA() {
        return cProdANVISA;
    }

    public void setcProdANVISA(String cProdANVISA) {
        this.cProdANVISA = cProdANVISA;
    }

    public double getvPMC() {
        return vPMC;
    }

    public void setvPMC(double vPMC) {
        this.vPMC = vPMC;
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
        return this.dVal;
    }

    public void setdVal(Date dVal) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dVal = sdf.format(dVal);
    }

    public String getxMotivoIsencao() {
        return xMotivoIsencao;
    }

    public void setxMotivoIsencao(String xMotivoIsencao) {
        this.xMotivoIsencao = xMotivoIsencao;
    }
}
