package br.com.acbr.lib.nfe.notafiscal;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ISSQNtotNFe {
    private BigDecimal vServ;
    private BigDecimal vBC;
    private BigDecimal vISS;
    private BigDecimal vPIS;
    private BigDecimal vCOFINS;
    private String dCompet;
    private BigDecimal vDeducao;
    private BigDecimal vOutro;
    private BigDecimal vDescIncond;
    private BigDecimal vDescCond;
    private BigDecimal vISSRet;
    private RegTribISSQN cRegTrib = RegTribISSQN.RTISSNenhum;

    public ISSQNtotNFe() {}

    public BigDecimal getVServ() {
        return vServ;
    }

    public void setVServ(BigDecimal vServ) {
        this.vServ = vServ;
    }

    public BigDecimal getVBC() {
        return vBC;
    }

    public void setVBC(BigDecimal vBC) {
        this.vBC = vBC;
    }

    public BigDecimal getVISS() {
        return vISS;
    }

    public void setVISS(BigDecimal vISS) {
        this.vISS = vISS;
    }

    public BigDecimal getVPIS() {
        return vPIS;
    }

    public void setVPIS(BigDecimal vPIS) {
        this.vPIS = vPIS;
    }

    public BigDecimal getVCOFINS() {
        return vCOFINS;
    }

    public void setVCOFINS(BigDecimal vCOFINS) {
        this.vCOFINS = vCOFINS;
    }

    public String getDCompet() {
        return this.dCompet;
    }

    public void setDCompet(Date dCompet) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dCompet = sdf.format(dCompet);
    }

    public BigDecimal getVDeducao() {
        return vDeducao;
    }

    public void setVDeducao(BigDecimal vDeducao) {
        this.vDeducao = vDeducao;
    }

    public BigDecimal getVOutro() {
        return vOutro;
    }

    public void setVOutro(BigDecimal vOutro) {
        this.vOutro = vOutro;
    }

    public BigDecimal getVDescIncond() {
        return vDescIncond;
    }

    public void setVDescIncond(BigDecimal vDescIncond) {
        this.vDescIncond = vDescIncond;
    }

    public BigDecimal getVDescCond() {
        return vDescCond;
    }

    public void setVDescCond(BigDecimal vDescCond) {
        this.vDescCond = vDescCond;
    }

    public BigDecimal getVISSRet() {
        return vISSRet;
    }

    public void setVISSRet(BigDecimal vISSRet) {
        this.vISSRet = vISSRet;
    }

    public RegTribISSQN getCRegTrib() {
        return cRegTrib;
    }

    public void setCRegTrib(RegTribISSQN cRegTrib) {
        if (cRegTrib != null) {
            this.cRegTrib = cRegTrib;
        } else {
            this.cRegTrib = RegTribISSQN.RTISSNenhum;
        }
    }
}
