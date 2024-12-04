package br.com.acbr.lib.nfe.notafiscal;

public class DetExportNFe {
    private String nDraw;
    private String nRE;
    private String chNFe;
    private double qExport;

    public DetExportNFe() {
    }

    public String getnDraw() {
        return nDraw;
    }

    public void setnDraw(String nDraw) {
        this.nDraw = nDraw;
    }

    public String getnRE() {
        return nRE;
    }

    public void setnRE(String nRE) {
        this.nRE = nRE;
    }

    public String getChNFe() {
        return chNFe;
    }

    public void setChNFe(String chNFe) {
        this.chNFe = chNFe;
    }

    public double getqExport() {
        return qExport;
    }

    public void setqExport(double qExport) {
        this.qExport = qExport;
    }
}
