package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;

import br.com.acbr.lib.comum.dfe.OrigemMercadoria;

public class IPIProdutoNFe {
    private CSTIPI cst = CSTIPI.ipi00;
    private String clEnq;
    private String cnpjProd;
    private String cSelo;
    private int qSelo;
    private String cEnq;
    private BigDecimal vBC;
    private BigDecimal qUnid;
    private BigDecimal vUnid;
    private BigDecimal pIPI;
    private BigDecimal vIPI;

    public IPIProdutoNFe() {
    }

    public CSTIPI getCst() {
        return cst;
    }

    public void setCst(CSTIPI cst) {
        if(cst != null){
            this.cst = cst;
        } else {
            this.cst = CSTIPI.ipi00;
        }
    }

    public String getClEnq() {
        return clEnq;
    }

    public void setClEnq(String clEnq) {
        this.clEnq = clEnq;
    }

    public String getCnpjProd() {
        return cnpjProd;
    }

    public void setCnpjProd(String cnpjProd) {
        this.cnpjProd = cnpjProd;
    }

    public String getCSelo() {
        return cSelo;
    }

    public void setCSelo(String cSelo) {
        this.cSelo = cSelo;
    }

    public int getQSelo() {
        return qSelo;
    }

    public void setQSelo(int qSelo) {
        this.qSelo = qSelo;
    }

    public String getCEnq() {
        return cEnq;
    }

    public void setCEnq(String cEnq) {
        this.cEnq = cEnq;
    }

    public BigDecimal getVBC() {
        return vBC;
    }

    public void setVBC(BigDecimal vBC) {
        this.vBC = vBC;
    }

    public BigDecimal getQUnid() {
        return qUnid;
    }

    public void setQUnid(BigDecimal qUnid) {
        this.qUnid = qUnid;
    }

    public BigDecimal getVUnid() {
        return vUnid;
    }

    public void setVUnid(BigDecimal vUnid) {
        this.vUnid = vUnid;
    }

    public BigDecimal getPIPI() {
        return pIPI;
    }

    public void setPIPI(BigDecimal pIPI) {
        this.pIPI = pIPI;
    }

    public BigDecimal getVIPI() {
        return vIPI;
    }

    public void setVIPI(BigDecimal vIPI) {
        this.vIPI = vIPI;
    }
}
