package br.com.acbr.lib.nfe.notafiscal;

public class ArmaNFe {
    private String nSerie;
    private TipoArma tpArma;
    private String nCano;
    private String descr;

    public ArmaNFe() {
    }

    public String getnSerie() {
        return nSerie;
    }

    public void setnSerie(String nSerie) {
        this.nSerie = nSerie;
    }

    public TipoArma getTpArma() {
        return tpArma;
    }

    public void setTpArma(TipoArma tpArma) {
        if(tpArma != null){
            this.tpArma = tpArma;
        } else {
            this.tpArma = TipoArma.taUsoPermitido;
        }
    }

    public String getnCano() {
        return nCano;
    }

    public void setnCano(String nCano) {
        this.nCano = nCano;
    }

    public String getDescr() {
        return descr;
    }

    public void setDescr(String descr) {
        this.descr = descr;
    }
}
