package br.com.acbr.lib.nfe.notafiscal;

import br.com.acbr.lib.comum.dfe.CSTPIS;

public class PISProdutoNFe {
    private CSTPIS cst;
    private Double vBC;
    private Double pPIS;
    private Double qBCProd;
    private Double vAliqProd;
    private Double vPIS;

    public PISProdutoNFe() {
    }

    public CSTPIS getCST() {
        return cst;
    }

    public void setCST(CSTPIS cst) {
        if(cst != null){
            this.cst = cst;
        } else {
            this.cst = CSTPIS.pis01;
        }
    }

    public Double getVBC() {
        return vBC;
    }

    public void setVBC(Double vBC) {
        this.vBC = vBC;
    }

    public Double getPPIS() {
        return pPIS;
    }

    public void setPPIS(Double pPIS) {
        this.pPIS = pPIS;
    }

    public Double getQBCProd() {
        return qBCProd;
    }

    public void setQBCProd(Double qBCProd) {
        this.qBCProd = qBCProd;
    }

    public Double getVAliqProd() {
        return vAliqProd;
    }

    public void setVAliqProd(Double vAliqProd) {
        this.vAliqProd = vAliqProd;
    }

    public Double getVPIS() {
        return vPIS;
    }

    public void setVPIS(Double vPIS) {
        this.vPIS = vPIS;
    }
}
