package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;

public class PagamentoNFe {

    private IndicadorPagamento indPag;
    private FormaPagamento tPag;
    private String xPag;
    private BigDecimal vPag;
    private String dPag;
    private String CNPJPag;
    private String UFPag;
    private TpIntegra tpIntegra;
    private String CNPJ;
    private BandeiraCartao tBand;
    private String cAut;
    private String CNPJReceb;
    private String idTermPag;
    private BigDecimal vTroco;

    public PagamentoNFe() {}

    public IndicadorPagamento getIndPag() {
        return indPag;
    }

    public void setIndPag(IndicadorPagamento indPag) {
        if (indPag != null) {
            this.indPag = indPag;
        } else {
            this.indPag = IndicadorPagamento.ipNenhum;
        }
    }

    public FormaPagamento gettPag() {
        return tPag;
    }

    public void settPag(FormaPagamento tPag) {
        if (tPag != null) {
            this.tPag = tPag;
        } else {
            this.tPag = FormaPagamento.fpDinheiro;
        }
    }

    public String getxPag() {
        return xPag;
    }

    public void setxPag(String xPag) {
        this.xPag = xPag;
    }

    public BigDecimal getvPag() {
        return vPag;
    }

    public void setvPag(BigDecimal vPag) {
        this.vPag = vPag;
    }

    public String getdPag() {
        return this.dPag;
    }

    public void setdPag(Date dPag) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dPag = sdf.format(dPag);
    }

    public String getCNPJPag() {
        return CNPJPag;
    }

    public void setCNPJPag(String CNPJPag) {
        this.CNPJPag = CNPJPag;
    }

    public String getUFPag() {
        return UFPag;
    }

    public void setUFPag(String UFPag) {
        this.UFPag = UFPag;
    }

    public TpIntegra getTpIntegra() {
        return tpIntegra;
    }

    public void setTpIntegra(TpIntegra tpIntegra) {
        if(tpIntegra != null){
            this.tpIntegra = tpIntegra;
        } else {
            this.tpIntegra = TpIntegra.tiNaoInformado;
        }
    }

    public String getCNPJ() {
        return CNPJ;
    }

    public void setCNPJ(String CNPJ) {
        this.CNPJ = CNPJ;
    }

    public BandeiraCartao gettBand() {
        return tBand;
    }

    public void settBand(BandeiraCartao tBand) {
        if (tBand != null) {
            this.tBand = tBand;
        } else {
            this.tBand = BandeiraCartao.bcOutros;
        }
    }

    public String getcAut() {
        return cAut;
    }

    public void setcAut(String cAut) {
        this.cAut = cAut;
    }

    public String getCNPJReceb() {
        return CNPJReceb;
    }

    public void setCNPJReceb(String CNPJReceb) {
        this.CNPJReceb = CNPJReceb;
    }

    public String getIdTermPag() {
        return idTermPag;
    }

    public void setIdTermPag(String idTermPag) {
        this.idTermPag = idTermPag;
    }

    public BigDecimal getvTroco() {
        return vTroco;
    }

    public void setvTroco(BigDecimal vTroco) {
        this.vTroco = vTroco;
    }
}
