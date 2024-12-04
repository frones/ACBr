package br.com.acbr.lib.nfe.notafiscal;

import br.com.acbr.lib.comum.dfe.CSTCofins;

public class COFINSProdutoNFe {
    private CSTCofins cst;
    private Double vBC;
    private Double pCOFINS;
    private Double qBCProd;
    private Double vAliqProd;
    private Double vCOFINS;

    public COFINSProdutoNFe() {
    }

    public CSTCofins getCst() {
        return cst;
    }

    public void setCst(CSTCofins cst) {
        if (cst != null){
            this.cst = cst;
        } else {
            this.cst = CSTCofins.cof01;
        }
    }

    public Double getvBC() {
        return vBC;
    }

    public void setvBC(Double vBC) {
        this.vBC = vBC;
    }

    public Double getpCOFINS() {
        return pCOFINS;
    }

    public void setpCOFINS(Double pCOFINS) {
        this.pCOFINS = pCOFINS;
    }

    public Double getqBCProd() {
        return qBCProd;
    }

    public void setqBCProd(Double qBCProd) {
        this.qBCProd = qBCProd;
    }

    public Double getvAliqProd() {
        return vAliqProd;
    }

    public void setvAliqProd(Double vAliqProd) {
        this.vAliqProd = vAliqProd;
    }

    public Double getvCOFINS() {
        return vCOFINS;
    }

    public void setvCOFINS(Double vCOFINS) {
        this.vCOFINS = vCOFINS;
    }
}
