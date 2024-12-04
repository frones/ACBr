package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DuplicataNFe {
    private String nDup;
    private String dVenc;
    private BigDecimal vDup;

    public DuplicataNFe() {}

    public String getNDup() {
        return nDup;
    }

    public void setNDup(String nDup) {
        this.nDup = nDup;
    }

    public String getDVenc() {
        return this.dVenc;
    }

    public void setDVenc(Date dVenc) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dVenc = sdf.format(dVenc);
    }

    public BigDecimal getVDup() {
        return vDup;
    }

    public void setVDup(BigDecimal vDup) {
        this.vDup = vDup;
    }
}
