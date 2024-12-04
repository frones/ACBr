package br.com.acbr.lib.nfe.notafiscal;

import java.util.ArrayList;

public class EncerranteNFe {
    private int nBico;
    private int nBomba;
    private int nTanque;
    private double vEncIni;
    private double vEncFin;

    public EncerranteNFe() {
    }

    public int getNBico() {
        return nBico;
    }

    public void setNBico(int nBico) {
        this.nBico = nBico;
    }

    public int getNBomba() {
        return nBomba;
    }

    public void setNBomba(int nBomba) {
        this.nBomba = nBomba;
    }

    public int getNTanque() {
        return nTanque;
    }

    public void setNTanque(int nTanque) {
        this.nTanque = nTanque;
    }

    public double getVEncIni() {
        return vEncIni;
    }

    public void setVEncIni(double vEncIni) {
        this.vEncIni = vEncIni;
    }

    public double getVEncFin() {
        return vEncFin;
    }

    public void setVEncFin(double vEncFin) {
        this.vEncFin = vEncFin;
    }
}
