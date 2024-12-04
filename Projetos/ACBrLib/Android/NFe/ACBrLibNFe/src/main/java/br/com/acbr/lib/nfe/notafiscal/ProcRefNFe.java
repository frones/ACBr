package br.com.acbr.lib.nfe.notafiscal;

public class ProcRefNFe {
    private String nProc;
    private IndicadorProcesso indProc;

    public ProcRefNFe() {}

    public String getnProc() {
        return nProc;
    }

    public void setnProc(String nProc) {
        this.nProc = nProc;
    }

    public IndicadorProcesso getIndProc() {
        return indProc;
    }

    public void setIndProc(IndicadorProcesso indProc) {
        this.indProc = indProc;
    }
}
