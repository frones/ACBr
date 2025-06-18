package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;

public class ISProdutoNFe {

    private CSTIS CSTIS;
    private TipoClassTribIS cClassTribIS;
    private BigDecimal vBCIS;
    private BigDecimal pIS;
    private BigDecimal pISEspec;
    private String uTrib;
    private BigDecimal qTrib;
    private BigDecimal vIS;

    public ISProdutoNFe(){

    }

    public CSTIS getCSTIS() {
        return CSTIS;
    }

    public void setCSTIS(CSTIS CSTIS) {
        this.CSTIS = CSTIS;
    }

    public TipoClassTribIS getcClassTribIS() {
        return cClassTribIS;
    }

    public void setcClassTribIS(TipoClassTribIS cClassTribIS) {
        this.cClassTribIS = cClassTribIS;
    }

    public BigDecimal getvBCIS() {
        return vBCIS;
    }

    public void setvBCIS(BigDecimal vBCIS) {
        this.vBCIS = vBCIS;
    }

    public BigDecimal getpIS() {
        return pIS;
    }

    public void setpIS(BigDecimal pIS) {
        this.pIS = pIS;
    }

    public BigDecimal getpISEspec() {
        return pISEspec;
    }

    public void setpISEspec(BigDecimal pISEspec) {
        this.pISEspec = pISEspec;
    }

    public String getuTrib() {
        return uTrib;
    }

    public void setuTrib(String uTrib) {
        this.uTrib = uTrib;
    }

    public BigDecimal getqTrib() {
        return qTrib;
    }

    public void setqTrib(BigDecimal qTrib) {
        this.qTrib = qTrib;
    }

    public BigDecimal getvIS() {
        return vIS;
    }

    public void setvIS(BigDecimal vIS) {
        this.vIS = vIS;
    }
}
