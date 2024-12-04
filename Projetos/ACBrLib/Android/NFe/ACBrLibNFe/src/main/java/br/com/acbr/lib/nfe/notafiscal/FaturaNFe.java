package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;

public class FaturaNFe {
    private String nFat;
    private BigDecimal vOrig;
    private BigDecimal vDesc;
    private BigDecimal vLiq;

    public FaturaNFe() {}

    public String getNFat() {
        return nFat;
    }

    public void setNFat(String nFat) {
        this.nFat = nFat;
    }

    public BigDecimal getVOrig() {
        return vOrig;
    }

    public void setVOrig(BigDecimal vOrig) {
        this.vOrig = vOrig;
    }

    public BigDecimal getVDesc() {
        return vDesc;
    }

    public void setVDesc(BigDecimal vDesc) {
        this.vDesc = vDesc;
    }

    public BigDecimal getVLiq() {
        return vLiq;
    }

    public void setVLiq(BigDecimal vLiq) {
        this.vLiq = vLiq;
    }
}
