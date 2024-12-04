package br.com.acbr.lib.nfe.notafiscal;

import br.com.acbr.lib.comum.dfe.IndicadorIE;

public class DestinatarioNFe {

    private String idEstrangeiro;
    private String CNPJCPF;
    private String xNome;
    private IndicadorIE indIEDest;
    private String IE;
    private String IM;
    private String ISUF;
    private String Email;
    private String xLgr;
    private String nro;
    private String xCpl;
    private String xBairro;
    private int cMun;
    private String xMun;
    private String UF;
    private String CEP;
    private int cPais;
    private String xPais;
    private String Fone;

    public DestinatarioNFe() {}

    public String getIdEstrangeiro() {
        return idEstrangeiro;
    }

    public void setIdEstrangeiro(String idEstrangeiro) {
        this.idEstrangeiro = idEstrangeiro;
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

    public IndicadorIE getIndIEDest() {
        return indIEDest;
    }

    public void setIndIEDest(IndicadorIE indIEDest) {
        if(indIEDest != null){
            this.indIEDest = IndicadorIE.fromValue(indIEDest.getValue());
        } else {
            this.indIEDest = IndicadorIE.inNaoContribuinte;
        }
    }

    public String getIE() {
        return IE;
    }

    public void setIE(String IE) {
        this.IE = IE;
    }

    public String getIM() {
        return IM;
    }

    public void setIM(String IM) {
        this.IM = IM;
    }

    public String getISUF() {
        return ISUF;
    }

    public void setISUF(String ISUF) {
        this.ISUF = ISUF;
    }

    public String getEmail() {
        return Email;
    }

    public void setEmail(String email) {
        Email = email;
    }

    public String getXLgr() {
        return xLgr;
    }

    public void setXLgr(String xLgr) {
        this.xLgr = xLgr;
    }

    public String getNro() {
        return nro;
    }

    public void setNro(String nro) {
        this.nro = nro;
    }

    public String getXCpl() {
        return xCpl;
    }

    public void setXCpl(String xCpl) {
        this.xCpl = xCpl;
    }

    public String getXBairro() {
        return xBairro;
    }

    public void setXBairro(String xBairro) {
        this.xBairro = xBairro;
    }

    public int getCMun() {
        return cMun;
    }

    public void setCMun(int cMun) {
        this.cMun = cMun;
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

    public String getCEP() {
        return CEP;
    }

    public void setCEP(String CEP) {
        this.CEP = CEP;
    }

    public int getCPais() {
        return cPais;
    }

    public void setCPais(int cPais) {
        this.cPais = cPais;
    }

    public String getXPais() {
        return xPais;
    }

    public void setXPais(String xPais) {
        this.xPais = xPais;
    }

    public String getFone() {
        return Fone;
    }

    public void setFone(String fone) {
        this.Fone = fone;
    }
}
