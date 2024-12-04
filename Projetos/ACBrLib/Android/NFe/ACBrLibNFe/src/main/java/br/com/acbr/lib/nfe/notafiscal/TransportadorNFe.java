package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class TransportadorNFe {
    private ModalidadeFrete modFrete;
    private String CNPJCPF;
    private String xNome;
    private String IE;
    private String xEnder;
    private String xMun;
    private String UF;
    private BigDecimal vServ;
    private BigDecimal vBCRet;
    private BigDecimal pICMSRet;
    private BigDecimal vICMSRet;
    private String CFOP;
    private int cMunFG;
    private String Placa;
    private String UFPlaca;
    private String RNTC;
    private String vagao;
    private String balsa;
    private List<ReboqueNFe> Reboque = new ArrayList<>();

    public TransportadorNFe() {}

    public ModalidadeFrete getModFrete() {
        return modFrete;
    }

    public void setModFrete(ModalidadeFrete modFrete) {
        if(modFrete != null){
            this.modFrete = modFrete;
        }else {
            this.modFrete = ModalidadeFrete.mfSemFrete;
        }
    }

    public String getCNPJCPF() {
        return CNPJCPF;
    }

    public void setCNPJCPF(String CNPJCPF) {
        this.CNPJCPF = CNPJCPF;
    }

    public String getXNome() {
        return xNome;
    }

    public void setXNome(String xNome) {
        this.xNome = xNome;
    }

    public String getIE() {
        return IE;
    }

    public void setIE(String IE) {
        this.IE = IE;
    }

    public String getXEnder() {
        return xEnder;
    }

    public void setXEnder(String xEnder) {
        this.xEnder = xEnder;
    }

    public String getXMun() {
        return xMun;
    }

    public void setXMun(String xMun) {
        this.xMun = xMun;
    }

    public String getUF() {
        return UF;
    }

    public void setUF(String UF) {
        this.UF = UF;
    }

    public BigDecimal getVServ() {
        return vServ;
    }

    public void setVServ(BigDecimal vServ) {
        this.vServ = vServ;
    }

    public BigDecimal getVBCRet() {
        return vBCRet;
    }

    public void setVBCRet(BigDecimal vBCRet) {
        this.vBCRet = vBCRet;
    }

    public BigDecimal getPICMSRet() {
        return pICMSRet;
    }

    public void setPICMSRet(BigDecimal pICMSRet) {
        this.pICMSRet = pICMSRet;
    }

    public BigDecimal getVICMSRet() {
        return vICMSRet;
    }

    public void setVICMSRet(BigDecimal vICMSRet) {
        this.vICMSRet = vICMSRet;
    }

    public String getCFOP() {
        return CFOP;
    }

    public void setCFOP(String CFOP) {
        this.CFOP = CFOP;
    }

    public int getCMunFG() {
        return cMunFG;
    }

    public void setCMunFG(int cMunFG) {
        this.cMunFG = cMunFG;
    }

    public String getPlaca() {
        return Placa;
    }

    public void setPlaca(String Placa) {
        this.Placa = Placa;
    }

    public String getUFPlaca() {
        return UFPlaca;
    }

    public void setUFPlaca(String UFPlaca) {
        this.UFPlaca = UFPlaca;
    }

    public String getRNTC() {
        return RNTC;
    }

    public void setRNTC(String RNTC) {
        this.RNTC = RNTC;
    }

    public String getVagao() {
        return vagao;
    }

    public void setVagao(String vagao) {
        this.vagao = vagao;
    }

    public String getBalsa() {
        return balsa;
    }

    public void setBalsa(String balsa) {
        this.balsa = balsa;
    }

    public List<ReboqueNFe> getReboque() {
        return Reboque;
    }

    public void setReboque(List<ReboqueNFe> Reboque) {
        this.Reboque = Reboque;
    }
}
