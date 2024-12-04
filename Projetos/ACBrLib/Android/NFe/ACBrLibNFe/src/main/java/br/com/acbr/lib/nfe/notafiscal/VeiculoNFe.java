package br.com.acbr.lib.nfe.notafiscal;

public class VeiculoNFe {
    private String chassi;
    private TipoOperacao tpOP;
    private String cCor;
    private String xCor;
    private String pot;
    private String cilin;
    private String pesoL;
    private String pesoB;
    private String nSerie;
    private String tpComb;
    private String nMotor;
    private String cmt;
    private String dist;
    private int anoMod;
    private int anoFab;
    private String tpPint;
    private int tpVeic;
    private int espVeic;
    private String vin;
    private CondicaoVeiculo condVeic;
    private String cMod;
    private String cCorDENATRAN;
    private int lota;
    private int tpRest;

    public VeiculoNFe() {}

    public String getChassi() {
        return chassi;
    }

    public void setChassi(String chassi) {
        this.chassi = chassi;
    }

    public TipoOperacao getTpOP() {
        return tpOP;
    }

    public void setTpOP(TipoOperacao tpOP) {
        if(tpOP != null){
            this.tpOP = tpOP;
        } else {
            this.tpOP = TipoOperacao.toOutros;
        }
    }

    public String getCCor() {
        return cCor;
    }

    public void setCCor(String cCor) {
        this.cCor = cCor;
    }

    public String getXCor() {
        return xCor;
    }

    public void setXCor(String xCor) {
        this.xCor = xCor;
    }

    public String getPot() {
        return pot;
    }

    public void setPot(String pot) {
        this.pot = pot;
    }

    public String getCilin() {
        return cilin;
    }

    public void setCilin(String cilin) {
        this.cilin = cilin;
    }

    public String getPesoL() {
        return pesoL;
    }

    public void setPesoL(String pesoL) {
        this.pesoL = pesoL;
    }

    public String getPesoB() {
        return pesoB;
    }

    public void setPesoB(String pesoB) {
        this.pesoB = pesoB;
    }

    public String getNSerie() {
        return nSerie;
    }

    public void setNSerie(String nSerie) {
        this.nSerie = nSerie;
    }

    public String getTpComb() {
        return tpComb;
    }

    public void setTpComb(String tpComb) {
        this.tpComb = tpComb;
    }

    public String getNMotor() {
        return nMotor;
    }

    public void setNMotor(String nMotor) {
        this.nMotor = nMotor;
    }

    public String getCmt() {
        return cmt;
    }

    public void setCmt(String cmt) {
        this.cmt = cmt;
    }

    public String getDist() {
        return dist;
    }

    public void setDist(String dist) {
        this.dist = dist;
    }

    public int getAnoMod() {
        return anoMod;
    }

    public void setAnoMod(int anoMod) {
        this.anoMod = anoMod;
    }

    public int getAnoFab() {
        return anoFab;
    }

    public void setAnoFab(int anoFab) {
        this.anoFab = anoFab;
    }

    public String getTpPint() {
        return tpPint;
    }

    public void setTpPint(String tpPint) {
        this.tpPint = tpPint;
    }

    public int getTpVeic() {
        return tpVeic;
    }

    public void setTpVeic(int tpVeic) {
        this.tpVeic = tpVeic;
    }

    public int getEspVeic() {
        return espVeic;
    }

    public void setEspVeic(int espVeic) {
        this.espVeic = espVeic;
    }

    public String getVin() {
        return vin;
    }

    public void setVin(String vin) {
        this.vin = vin;
    }

    public CondicaoVeiculo getCondVeic() {
        return condVeic;
    }

    public void setCondVeic(CondicaoVeiculo condVeic) {
        if(condVeic != null){
            this.condVeic = condVeic;
        } else {
            this.condVeic = CondicaoVeiculo.cvAcabado;
        }
    }

    public String getCMod() {
        return cMod;
    }

    public void setCMod(String cMod) {
        this.cMod = cMod;
    }

    public String getCCorDENATRAN() {
        return cCorDENATRAN;
    }

    public void setCCorDENATRAN(String cCorDENATRAN) {
        this.cCorDENATRAN = cCorDENATRAN;
    }

    public int getLota() {
        return lota;
    }

    public void setLota(int lota) {
        this.lota = lota;
    }

    public int getTpRest() {
        return tpRest;
    }

    public void setTpRest(int tpRest) {
        this.tpRest = tpRest;
    }
}
