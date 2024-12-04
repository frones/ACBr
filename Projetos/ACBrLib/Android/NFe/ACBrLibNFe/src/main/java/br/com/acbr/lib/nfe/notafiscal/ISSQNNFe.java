package br.com.acbr.lib.nfe.notafiscal;

import br.com.acbr.lib.comum.dfe.CSTCofins;
import br.com.acbr.lib.comum.dfe.IndIncentivo;

public class ISSQNNFe {
    private Double vBC;
    private double vAliq;
    private double vISSQN;
    private int cMunFG;
    private String cListServ;
    private ISSQNcSitTrib cSitTrib;
    private double vDeducao;
    private double vOutro;
    private double vDescIncond;
    private double vDescCond;
    private double vISSRet;
    private IndISS indISS;
    private String cServico;
    private String cMun;
    private String cPais;
    private String nProcesso;
    private IndIncentivo indIncentivo;

    public ISSQNNFe() {
    }

    public Double getvBC() { return vBC; }
    public void setvBC(Double vBC) { this.vBC = vBC; }

    public double getvAliq() { return vAliq; }
    public void setvAliq(Double vAliq) { this.vAliq = vAliq; }

    public Double getvISSQN() { return vISSQN; }
    public void setvISSQN(Double vISSQN) { this.vISSQN = vISSQN; }

    public int getcMunFG() { return cMunFG; }
    public void setcMunFG(int cMunFG) { this.cMunFG = cMunFG; }

    public String getcListServ() { return cListServ; }
    public void setcListServ(String cListServ) { this.cListServ = cListServ; }

    public ISSQNcSitTrib getcSitTrib() { return cSitTrib; }
    public void setcSitTrib(ISSQNcSitTrib cSitTrib)
    {
        if (cSitTrib != null){
            this.cSitTrib = cSitTrib;
        } else {
            this.cSitTrib = ISSQNcSitTrib.ISSQNcSitTribVazio;
        }
    }

    public Double getvDeducao() { return vDeducao; }
    public void setvDeducao(Double vDeducao) { this.vDeducao = vDeducao; }

    public Double getvOutro() { return vOutro; }
    public void setvOutro(Double vOutro) { this.vOutro = vOutro; }

    public Double getvDescIncond() { return vDescIncond; }
    public void setvDescIncond(Double vDescIncond) { this.vDescIncond = vDescIncond; }

    public Double getvDescCond() { return vDescCond; }
    public void setvDescCond(Double vDescCond) { this.vDescCond = vDescCond; }

    public Double getvISSRet() { return vISSRet; }
    public void setvISSRet(Double vISSRet) { this.vISSRet = vISSRet; }

    public IndISS getIndISS() { return indISS; }
    public void setIndISS(IndISS indISS)
    {
        if (indISS != null){
            this.indISS = indISS;
        } else {
            this.indISS = IndISS.iiExigivel;
        }
    }

    public String getcServico() { return cServico; }
    public void setcServico(String cServico) { this.cServico = cServico; }

    public String getcMun() { return cMun; }
    public void setcMun(String cMun) { this.cMun = cMun; }

    public String getcPais() { return cPais; }
    public void setcPais(String cPais) { this.cPais = cPais; }

    public String getnProcesso() { return nProcesso; }
    public void setnProcesso(String nProcesso) { this.nProcesso = nProcesso; }

    public IndIncentivo getIndIncentivo() { return indIncentivo; }
    public void setIndIncentivo(IndIncentivo indIncentivo)
    {
        if (indIncentivo != null){
            this.indIncentivo = indIncentivo;
        } else {
            this.indIncentivo = IndIncentivo.iiSim;
        }
    }
}
