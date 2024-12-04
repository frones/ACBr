package br.com.acbr.lib.nfe.notafiscal;

import java.text.SimpleDateFormat;
import java.util.Date;

import br.com.acbr.lib.comum.dfe.TipoAmbiente;
import br.com.acbr.lib.comum.ini.ACBrIniSection;

public class ProcNFe{

    private TipoAmbiente tpAmb;
    private String verAplic;
    private String chNFe;
    private String dhRecbto;
    private String nProt;
    private String digVal;
    private int cStat;
    private String xMotivo;

    public ProcNFe() {
    }

    public TipoAmbiente getTpAmb() {
        return tpAmb;
    }

    public void setTpAmb(TipoAmbiente tpAmb) {
        if (tpAmb != null){
            this.tpAmb = TipoAmbiente.fromValue(tpAmb.getValue());
        }else {
            this.tpAmb = TipoAmbiente.taHomologacao;
        }
    }

    public String getVerAplic() {
        return verAplic;
    }

    public void setVerAplic(String verAplic) {
        this.verAplic = verAplic;
    }

    public String getChNFe() {
        return chNFe;
    }

    public void setChNFe(String chNFe) {
        this.chNFe = chNFe;
    }

    public String getDhRecbto() {
        return this.dhRecbto;
    }

    public void setDhRecbto(Date dhRecbto) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dhRecbto = sdf.format(dhRecbto);
    }

    public String getNProt() {
        return nProt;
    }

    public void setNProt(String nProt) {
        this.nProt = nProt;
    }

    public String getDigVal() {
        return digVal;
    }

    public void setDigVal(String digVal) {
        this.digVal = digVal;
    }

    public int getCStat() {
        return cStat;
    }

    public void setCStat(int cStat) {
        this.cStat = cStat;
    }

    public String getXMotivo() {
        return xMotivo;
    }

    public void setXMotivo(String xMotivo) {
        this.xMotivo = xMotivo;
    }

}
