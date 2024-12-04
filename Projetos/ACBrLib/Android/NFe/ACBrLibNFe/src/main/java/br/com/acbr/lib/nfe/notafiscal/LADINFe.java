package br.com.acbr.lib.nfe.notafiscal;

public class LADINFe {
    private int nAdicao;
    private int nSeqAdi;
    private String cFabricante;
    private double vDescDI;
    private String nDraw;

    public LADINFe() {
    }

    public int getnAdicao() {
        return nAdicao;
    }

    public void setnAdicao(int nAdicao) {
        this.nAdicao = nAdicao;
    }

    public int getnSeqAdi() {
        return nSeqAdi;
    }

    public void setnSeqAdi(int nSeqAdi) {
        this.nSeqAdi = nSeqAdi;
    }

    public String getcFabricante() {
        return cFabricante;
    }

    public void setcFabricante(String cFabricante) {
        this.cFabricante = cFabricante;
    }

    public double getvDescDI() {
        return vDescDI;
    }

    public void setvDescDI(double vDescDI) {
        this.vDescDI = vDescDI;
    }

    public String getnDraw() {
        return nDraw;
    }

    public void setnDraw(String nDraw) {
        this.nDraw = nDraw;
    }
}
