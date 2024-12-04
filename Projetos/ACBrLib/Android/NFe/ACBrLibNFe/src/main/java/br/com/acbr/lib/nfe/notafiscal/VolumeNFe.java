package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class VolumeNFe {
    private int qVol;
    private String esp;
    private String Marca;
    private String nVol;
    private BigDecimal pesoL;
    private BigDecimal pesoB;
    private List<LacresNFe> Lacres = new ArrayList<>();

    public VolumeNFe() {}

    public int getQVol() {
        return qVol;
    }

    public void setQVol(int qVol) {
        this.qVol = qVol;
    }

    public String getEsp() {
        return esp;
    }

    public void setEsp(String esp) {
        this.esp = esp;
    }

    public String getMarca() {
        return Marca;
    }

    public void setMarca(String Marca) {
        this.Marca = Marca;
    }

    public String getNVol() {
        return nVol;
    }

    public void setNVol(String nVol) {
        this.nVol = nVol;
    }

    public BigDecimal getPesoL() {
        return pesoL;
    }

    public void setPesoL(BigDecimal pesoL) {
        this.pesoL = pesoL;
    }

    public BigDecimal getPesoB() {
        return pesoB;
    }

    public void setPesoB(BigDecimal pesoB) {
        this.pesoB = pesoB;
    }

    public List<LacresNFe> getLacres() {
        return Lacres;
    }

    public void setLacres(List<LacresNFe> Lacres) {
        this.Lacres = Lacres;
    }
}
