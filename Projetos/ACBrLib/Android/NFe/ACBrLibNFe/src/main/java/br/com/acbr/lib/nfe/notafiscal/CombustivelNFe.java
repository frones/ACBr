package br.com.acbr.lib.nfe.notafiscal;

import java.util.ArrayList;
import java.util.List;

public class CombustivelNFe {
    private int cProdANP;
    private double pMixGN;
    private String codif;
    private double qTemp;
    private String ufCons;
    private String descANP;
    private double pGLP;
    private double pGNn;
    private double pGNi;
    private double vPart;
    private double pBio;
    private CIDENFe cide;
    private EncerranteNFe encerrante;
    private List<OrigCombNFe> origComb;

    public CombustivelNFe() {
        this.cide = new CIDENFe();
        this.encerrante = new EncerranteNFe();
        this.origComb = new ArrayList<>();
    }

    public int getCProdANP() {
        return cProdANP;
    }

    public void setCProdANP(int cProdANP) {
        this.cProdANP = cProdANP;
    }

    public double getPMixGN() {
        return pMixGN;
    }

    public void setPMixGN(double pMixGN) {
        this.pMixGN = pMixGN;
    }

    public String getCodif() {
        return codif;
    }

    public void setCodif(String codif) {
        this.codif = codif;
    }

    public double getQTemp() {
        return qTemp;
    }

    public void setQTemp(double qTemp) {
        this.qTemp = qTemp;
    }

    public String getUfCons() {
        return ufCons;
    }

    public void setUfCons(String ufCons) {
        this.ufCons = ufCons;
    }

    public String getDescANP() {
        return descANP;
    }

    public void setDescANP(String descANP) {
        this.descANP = descANP;
    }

    public double getPGLP() {
        return pGLP;
    }

    public void setPGLP(double pGLP) {
        this.pGLP = pGLP;
    }

    public double getPGNn() {
        return pGNn;
    }

    public void setPGNn(double pGNn) {
        this.pGNn = pGNn;
    }

    public double getPGNi() {
        return pGNi;
    }

    public void setPGNi(double pGNi) {
        this.pGNi = pGNi;
    }

    public double getVPart() {
        return vPart;
    }

    public void setVPart(double vPart) {
        this.vPart = vPart;
    }

    public double getPBio() {
        return pBio;
    }

    public void setPBio(double pBio) {
        this.pBio = pBio;
    }

    public CIDENFe getCIDE() {
        return cide;
    }

    public void setCIDE(CIDENFe cide) {
        this.cide = cide;
    }

    public EncerranteNFe getEncerrante() {
        return encerrante;
    }

    public void setEncerrante(EncerranteNFe encerrante) {
        this.encerrante = encerrante;
    }

    public List<OrigCombNFe> getOrigComb() {
        return origComb;
    }
}
