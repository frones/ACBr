package br.com.acbr.lib.nfe.notafiscal;

public class NFRef {
    private TipoRef Tipo;
    private String refNFe;
    private String cUF;
    private String AAMM;
    private String CNPJ;
    private String mod;
    private String Serie;
    private String nNF;
    private String CNPJCPF;
    private String IE;
    private String refCTe;
    private String modECF;
    private String nECF;
    private String nCOO;

    public NFRef() {}

    public TipoRef getTipo() {
        return Tipo;
    }

    public void setTipo(TipoRef tipo) {
        this.Tipo = tipo;
    }

    public String getRefNFe() {
        return refNFe;
    }

    public void setRefNFe(String refNFe) {
        this.refNFe = refNFe;
    }

    public String getCUF() {
        return cUF;
    }

    public void setCUF(String cUF) {
        this.cUF = cUF;
    }

    public String getAAMM() {
        return AAMM;
    }

    public void setAAMM(String AAMM) {
        this.AAMM = AAMM;
    }

    public String getCNPJ() {
        return CNPJ;
    }

    public void setCNPJ(String CNPJ) {
        this.CNPJ = CNPJ;
    }

    public String getMod() {
        return mod;
    }

    public void setMod(String mod) {
        this.mod = mod;
    }

    public String getSerie() {
        return Serie;
    }

    public void setSerie(String serie) {
        this.Serie = serie;
    }

    public String getNNF() {
        return nNF;
    }

    public void setNNF(String nNF) {
        this.nNF = nNF;
    }

    public String getCNPJCPF() {
        return CNPJCPF;
    }

    public void setCNPJCPF(String CNPJCPF) {
        this.CNPJCPF = CNPJCPF;
    }

    public String getIE() {
        return IE;
    }

    public void setIE(String IE) {
        this.IE = IE;
    }

    public String getRefCTe() {
        return refCTe;
    }

    public void setRefCTe(String refCTe) {
        this.refCTe = refCTe;
    }

    public String getModECF() {
        return modECF;
    }

    public void setModECF(String modECF) {
        this.modECF = modECF;
    }

    public String getNECF() {
        return nECF;
    }

    public void setNECF(String nECF) {
        this.nECF = nECF;
    }

    public String getNCOO() {
        return nCOO;
    }

    public void setNCOO(String nCOO) {
        this.nCOO = nCOO;
    }
}
