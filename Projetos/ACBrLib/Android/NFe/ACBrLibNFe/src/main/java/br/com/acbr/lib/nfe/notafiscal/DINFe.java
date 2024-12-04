package br.com.acbr.lib.nfe.notafiscal;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

public class DINFe {
    private String nDi;
    private String dDi;
    private String xLocDesemb;
    private String ufDesemb;
    private String dDesemb;
    private TipoViaTransp tpViaTransp;
    private double vAFRMM;
    private String tpIntermedio;
    private String cnpj;
    private String ufTerceiro;
    private String cExportador;
    private List<LADINFe> ladi;

    public DINFe() {
    }

    public String getNDi() {
        return nDi;
    }

    public void setNDi(String nDi) {
        this.nDi = nDi;
    }

    public String getDDi() {
        return this.dDi;
    }

    public void setDDi(Date dDi) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dDi = sdf.format(dDi);
    }

    public String getXLocDesemb() {
        return xLocDesemb;
    }

    public void setXLocDesemb(String xLocDesemb) {
        this.xLocDesemb = xLocDesemb;
    }

    public String getUfDesemb() {
        return ufDesemb;
    }

    public void setUfDesemb(String ufDesemb) {
        this.ufDesemb = ufDesemb;
    }

    public String getDDesemb() {
        return this.dDesemb;
    }

    public void setDDesemb(Date dDesemb) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dDesemb = sdf.format(dDesemb);
    }

    public TipoViaTransp getTpViaTransp() {
        return tpViaTransp;
    }

    public void setTpViaTransp(TipoViaTransp tpViaTransp) {
        this.tpViaTransp = tpViaTransp;
    }

    public double getVAFRMM() {
        return vAFRMM;
    }

    public void setVAFRMM(double vAFRMM) {
        this.vAFRMM = vAFRMM;
    }

    public String getTpIntermedio() {
        return tpIntermedio;
    }

    public void setTpIntermedio(String tpIntermedio) {
        this.tpIntermedio = tpIntermedio;
    }

    public String getCnpj() {
        return cnpj;
    }

    public void setCnpj(String cnpj) {
        this.cnpj = cnpj;
    }

    public String getUfTerceiro() {
        return ufTerceiro;
    }

    public void setUfTerceiro(String ufTerceiro) {
        this.ufTerceiro = ufTerceiro;
    }

    public String getCExportador() {
        return cExportador;
    }

    public void setCExportador(String cExportador) {
        this.cExportador = cExportador;
    }

    public List<LADINFe> getLadi() {
        return ladi;
    }

    public void setLadi(List<LADINFe> ladi) {
        this.ladi = ladi;
    }
}
