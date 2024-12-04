package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;

public class AvulsaNFe {
    private String cnpj;
    private String xOrgao;
    private String matr;
    private String xAgente;
    private String fone;
    private String uf;
    private String nDAR;
    private String dEmi;
    private BigDecimal vDAR;
    private String repEmi;
    private String dPag;

    public AvulsaNFe() {}

    public String getCnpj() {
        return cnpj;
    }

    public void setCnpj(String cnpj) {
        this.cnpj = cnpj;
    }

    public String getxOrgao() {
        return xOrgao;
    }

    public void setxOrgao(String xOrgao) {
        this.xOrgao = xOrgao;
    }

    public String getMatr() {
        return matr;
    }

    public void setMatr(String matr) {
        this.matr = matr;
    }

    public String getxAgente() {
        return xAgente;
    }

    public void setxAgente(String xAgente) {
        this.xAgente = xAgente;
    }

    public String getFone() {
        return fone;
    }

    public void setFone(String fone) {
        this.fone = fone;
    }

    public String getUf() {
        return uf;
    }

    public void setUf(String uf) {
        this.uf = uf;
    }

    public String getNDAR() {
        return nDAR;
    }

    public void setNDAR(String nDAR) {
        this.nDAR = nDAR;
    }

    public String getDEmi() {
        return this.dEmi;
    }

    public void setDEmi(Date dEmi) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dEmi = sdf.format(dEmi);
    }

    public BigDecimal getVDAR() {
        return vDAR;
    }

    public void setVDAR(BigDecimal vDAR) {
        this.vDAR = vDAR;
    }

    public String getRepEmi() {
        return repEmi;
    }

    public void setRepEmi(String repEmi) {
        this.repEmi = repEmi;
    }

    public String getDPag() {
        return this.dPag;
    }

    public void setDPag(Date dPag) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dPag = sdf.format(dPag);
    }
}
