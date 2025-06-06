package br.com.acbr.lib.nfe.notafiscal;

public class DFeReferenciado {

    private String chaveAcesso;
    private int nItem;

    public DFeReferenciado(){

    }

    public String getChaveAcesso() {
        return chaveAcesso;
    }

    public void setChaveAcesso(String chaveAcesso) {
        this.chaveAcesso = chaveAcesso;
    }

    public int getnItem() {
        return nItem;
    }

    public void setnItem(int nItem) {
        this.nItem = nItem;
    }
}
