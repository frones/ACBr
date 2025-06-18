package br.com.acbr.lib.nfe.notafiscal;

import androidx.annotation.NonNull;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import br.com.acbr.lib.comum.dfe.TipoAmbiente;
import br.com.acbr.lib.comum.dfe.TipoEnteGov;
import br.com.acbr.lib.comum.dfe.TipoOperGov;
import br.com.acbr.lib.comum.dfe.gPagAntecipado;
import br.com.acbr.lib.nfe.ModeloDF;
import br.com.acbr.lib.nfe.TipoDANFE;
import br.com.acbr.lib.nfe.TipoEmissao;
import br.com.acbr.lib.nfe.TipoNFe;
import br.com.acbr.lib.nfe.TipoNFeCredito;
import br.com.acbr.lib.nfe.TipoNFeDebito;

public class IdentificacaoNFe {

    private int cUF;
    private int cNF;
    private String natOp;
    private IndicadorPagamento indPag;
    private ModeloDF modelo;
    private String Serie;
    private int nNF;
    private String dhEmi;
    private String dhSaiEnt;
    private TipoNFe tpNF;
    private DestinoOperacao idDest;
    private TipoDANFE tpImp;
    private TipoEmissao tpEmis;
    private TipoAmbiente tpAmb;
    private FinalidadeNFe finNFe;
    private ConsumidorFinal indFinal;
    private PresencaComprador indPres;
    private ProcessoEmissao procEmi;
    private IndIntermed indIntermed;
    private String verProc;
    private String dhCont;
    private String xJust;
    private List<NFRef> NFref = new ArrayList<>();
    private int cMunFG;
    private int cMunFGIBS;
    private TipoNFeDebito tpNFDebito;
    private TipoNFeCredito tpNFCredito;
    private TipoEnteGov tpEnteGov;
    private BigDecimal pRedutor;
    private TipoOperGov tpOperGov;
    private List<gPagAntecipado> gPagAntecipado = new ArrayList<>();

    public IdentificacaoNFe() {}

    public int getcUF() {
        return cUF;
    }

    public void setcUF(int cUF) {
        this.cUF = cUF;
    }

    public int getcNF() {
        return cNF;
    }

    public void setcNF(int cNF) {
        this.cNF = cNF;
    }

    public String getNatOp() {
        return natOp;
    }

    public void setNatOp(String natOp) {
        this.natOp = natOp;
    }

    public IndicadorPagamento getIndPag() {
        return indPag;
    }

    public void setIndPag(IndicadorPagamento indPag) {
        if (indPag != null) {
            this.indPag = IndicadorPagamento.fromValue(indPag.getValue());
        } else {
            this.indPag = IndicadorPagamento.ipNenhum;
        }
    }

    public ModeloDF getModelo() {
        return modelo;
    }

    public void setModelo(ModeloDF modelo) {
        if(modelo != null){
            this.modelo = ModeloDF.fromValue(modelo.getValue());
        } else {
            this.modelo = ModeloDF.moNFe;
        }
    }

    public String getSerie() {
        return Serie;
    }

    public void setSerie(String serie) {
        Serie = serie;
    }

    public int getnNF() {
        return nNF;
    }

    public void setnNF(int nNF) {
        this.nNF = nNF;
    }

    public String getDhEmi() {
        return this.dhEmi;
    }

    public void setDhEmi(Date dhEmi) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dhEmi = sdf.format(dhEmi);
    }

    public String getDhSaiEnt() {
        return this.dhSaiEnt;
    }

    public void setDhSaiEnt(Date dhSaiEnt) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dhSaiEnt = sdf.format(dhSaiEnt);
    }

    public TipoNFe getTpNF() {
        return tpNF;
    }

    public void setTpNF(TipoNFe tpNF) {
        if(tpNF != null){
            this.tpNF = TipoNFe.fromValue(tpNF.getValue());
        } else {
            this.tpNF = TipoNFe.tnSaida;
        }
    }

    public DestinoOperacao getIdDest() {
        return idDest;
    }

    public void setIdDest(DestinoOperacao idDest) {
        if(idDest != null){
            this.idDest = DestinoOperacao.fromValue(idDest.getValue());
        } else {
            this.idDest = DestinoOperacao.doInterna;
        }
    }

    public TipoDANFE getTpImp() {
        return tpImp;
    }

    public void setTpImp(TipoDANFE tpImp) {
        if(tpImp != null){
            this.tpImp = TipoDANFE.fromValue(tpImp.getValue());
        } else {
            this.tpImp = TipoDANFE.tiRetrato;
        }
    }

    public TipoEmissao getTpEmis() {
        return tpEmis;
    }

    public void setTpEmis(TipoEmissao tpEmis) {
        if(tpEmis != null){
            this.tpEmis = TipoEmissao.fromValue(tpEmis.getValue());
        } else {
            this.tpEmis = TipoEmissao.teNormal;
        }
    }

    public TipoAmbiente getTpAmb() {
        return tpAmb;
    }

    public void setTpAmb(TipoAmbiente tpAmb) {
        if(tpAmb != null){
            this.tpAmb = TipoAmbiente.fromValue(tpAmb.getValue());
        } else {
            this.tpAmb = TipoAmbiente.taHomologacao;
        }
    }

    public FinalidadeNFe getFinNFe() {
        return finNFe;
    }

    public void setFinNFe(FinalidadeNFe finNFe) {
        if(finNFe != null){
            this.finNFe = FinalidadeNFe.fromValue(finNFe.getValue());
        } else {
            this.finNFe = FinalidadeNFe.fnNormal;
        }
    }

    public ConsumidorFinal getIndFinal() {
        return indFinal;
    }

    public void setIndFinal(ConsumidorFinal indFinal) {
        if(indFinal != null){
            this.indFinal = ConsumidorFinal.fromValue(indFinal.getValue());
        } else {
            this.indFinal = ConsumidorFinal.cfNao;
        }
    }

    public PresencaComprador getIndPres() {
        return indPres;
    }

    public void setIndPres(PresencaComprador indPres) {
        if(indPres != null){
            this.indPres = PresencaComprador.fromValue(indPres.getValue());
        } else {
            this.indPres = PresencaComprador.pcPresencial;
        }
    }

    public ProcessoEmissao getProcEmi() {
        return procEmi;
    }

    public void setProcEmi(ProcessoEmissao procEmi) {
        if(procEmi != null){
            this.procEmi = ProcessoEmissao.fromValue(procEmi.getValue());
        } else {
            this.procEmi = ProcessoEmissao.peAplicativoContribuinte;
        }
    }

    public IndIntermed getIndIntermed() {
        return indIntermed;
    }

    public void setIndIntermed(IndIntermed indIntermed) {
        if(indIntermed != null){
            this.indIntermed = IndIntermed.fromValue(indIntermed.getValue());
        } else {
            this.indIntermed = IndIntermed.iiSemOperacao;
        }
    }

    public String getVerProc() {
        return verProc;
    }

    public void setVerProc(String verProc) {
        this.verProc = verProc;
    }

    public String getDhCont() {
        return this.dhCont;
    }

    public void setDhCont(Date dhCont) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        this.dhCont = sdf.format(dhCont);
    }

    public String getxJust() {
        return xJust;
    }

    public void setxJust(String xJust) {
        this.xJust = xJust;
    }

    public List<NFRef> getNFref() {
        return NFref;
    }

    public void setNFref(List<NFRef> NFref) {
        this.NFref = NFref;
    }

    public int getcMunFG() {
        return cMunFG;
    }

    public void setcMunFG(int cMunFG) {
        this.cMunFG = cMunFG;
    }

    public int getcMunFGIBS() {
        return cMunFGIBS;
    }

    public void setcMunFGIBS(int cMunFGIBS) {
        this.cMunFGIBS = cMunFGIBS;
    }

    public TipoNFeDebito getTpNFDebito() {
        return tpNFDebito;
    }

    public void setTpNFDebito(TipoNFeDebito tpNFDebito) {
        this.tpNFDebito = tpNFDebito;
    }

    public TipoNFeCredito getTpNFCredito() {
        return tpNFCredito;
    }

    public void setTpNFCredito(TipoNFeCredito tpNFCredito) {
        this.tpNFCredito = tpNFCredito;
    }

    public TipoEnteGov getTpEnteGov() {
        return tpEnteGov;
    }

    public void setTpEnteGov(TipoEnteGov tpEnteGov) {
        this.tpEnteGov = tpEnteGov;
    }

    public BigDecimal getpRedutor() {
        return pRedutor;
    }

    public void setpRedutor(BigDecimal pRedutor) {
        this.pRedutor = pRedutor;
    }

    public TipoOperGov getTpOperGov() {
        return tpOperGov;
    }

    public void setTpOperGov(TipoOperGov tpOperGov) {
        this.tpOperGov = tpOperGov;
    }

    public List<gPagAntecipado> getgPagAntecipado() {
        return gPagAntecipado;
    }

    public void setgPagAntecipado(List<gPagAntecipado> gPagAntecipado) {
        this.gPagAntecipado = gPagAntecipado;
    }
}
