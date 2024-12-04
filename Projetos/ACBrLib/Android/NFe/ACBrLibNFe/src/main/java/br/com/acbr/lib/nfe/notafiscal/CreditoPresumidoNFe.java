package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;

public class CreditoPresumidoNFe {
    private String cCredPresumido;
    private BigDecimal pCredPresumido;
    private BigDecimal vCredPresumido;
    private String cBenefRBC;

    public String getCCredPresumido() {
        return cCredPresumido;
    }

    public void setCCredPresumido(String cCredPresumido) {
        this.cCredPresumido = cCredPresumido;
    }

    public BigDecimal getPCredPresumido() {
        return pCredPresumido;
    }

    public void setPCredPresumido(BigDecimal pCredPresumido) {
        this.pCredPresumido = pCredPresumido;
    }

    public BigDecimal getVCredPresumido() {
        return vCredPresumido;
    }

    public void setVCredPresumido(BigDecimal vCredPresumido) {
        this.vCredPresumido = vCredPresumido;
    }

    public String getCBenefRBC() {
        return cBenefRBC;
    }

    public void setCBenefRBC(String cBenefRBC) {
        this.cBenefRBC = cBenefRBC;
    }
}
