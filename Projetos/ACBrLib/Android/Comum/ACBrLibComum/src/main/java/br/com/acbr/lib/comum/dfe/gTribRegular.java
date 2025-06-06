package br.com.acbr.lib.comum.dfe;

import java.math.BigDecimal;

public class gTribRegular {

    private CSTIBSCBS CSTReg;
    private cClassTribIBSCBS cClassTribReg;
    private BigDecimal pAliqEfetRegIBSUF;
    private BigDecimal vTribRegIBSUF;
    private BigDecimal pAliqEfetRegIBSMun;
    private BigDecimal vTribRegIBSMun;
    private BigDecimal pAliqEfetRegCBS;
    private BigDecimal vTribRegCBS;

    public gTribRegular() {
    }

    public CSTIBSCBS getCSTReg() {
        return CSTReg;
    }

    public void setCSTReg(CSTIBSCBS CSTReg) {
        this.CSTReg = CSTReg;
    }

    public cClassTribIBSCBS getcClassTribReg() {
        return cClassTribReg;
    }

    public void setcClassTribReg(cClassTribIBSCBS cClassTribReg) {
        this.cClassTribReg = cClassTribReg;
    }

    public BigDecimal getpAliqEfetRegIBSUF() {
        return pAliqEfetRegIBSUF;
    }

    public void setpAliqEfetRegIBSUF(BigDecimal pAliqEfetRegIBSUF) {
        this.pAliqEfetRegIBSUF = pAliqEfetRegIBSUF;
    }

    public BigDecimal getvTribRegIBSUF() {
        return vTribRegIBSUF;
    }

    public void setvTribRegIBSUF(BigDecimal vTribRegIBSUF) {
        this.vTribRegIBSUF = vTribRegIBSUF;
    }

    public BigDecimal getpAliqEfetRegIBSMun() {
        return pAliqEfetRegIBSMun;
    }

    public void setpAliqEfetRegIBSMun(BigDecimal pAliqEfetRegIBSMun) {
        this.pAliqEfetRegIBSMun = pAliqEfetRegIBSMun;
    }

    public BigDecimal getvTribRegIBSMun() {
        return vTribRegIBSMun;
    }

    public void setvTribRegIBSMun(BigDecimal vTribRegIBSMun) {
        this.vTribRegIBSMun = vTribRegIBSMun;
    }

    public BigDecimal getpAliqEfetRegCBS() {
        return pAliqEfetRegCBS;
    }

    public void setpAliqEfetRegCBS(BigDecimal pAliqEfetRegCBS) {
        this.pAliqEfetRegCBS = pAliqEfetRegCBS;
    }

    public BigDecimal getvTribRegCBS() {
        return vTribRegCBS;
    }

    public void setvTribRegCBS(BigDecimal vTribRegCBS) {
        this.vTribRegCBS = vTribRegCBS;
    }
}
