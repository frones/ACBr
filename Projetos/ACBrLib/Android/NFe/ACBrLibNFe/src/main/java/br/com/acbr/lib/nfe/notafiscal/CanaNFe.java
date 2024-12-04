package br.com.acbr.lib.nfe.notafiscal;

import java.util.ArrayList;
import java.util.List;

public class CanaNFe {

    private String safra;
    private String ref;
    private double qTotMes;
    private double qTotAnt;
    private double qTotGer;
    private double vFor;
    private double vTotDed;
    private double vLiqFor;
    private List<ForDiaNFe> forDia = new ArrayList<>();
    private List<DeducNFe> deduc = new ArrayList<>();

    public CanaNFe() {}

    public String getSafra() {
        return safra;
    }

    public void setSafra(String safra) {
        this.safra = safra;
    }

    public String getRef() {
        return ref;
    }

    public void setRef(String ref) {
        this.ref = ref;
    }

    public double getQTotMes() {
        return qTotMes;
    }

    public void setQTotMes(double qTotMes) {
        this.qTotMes = qTotMes;
    }

    public double getQTotAnt() {
        return qTotAnt;
    }

    public void setQTotAnt(double qTotAnt) {
        this.qTotAnt = qTotAnt;
    }

    public double getQTotGer() {
        return qTotGer;
    }

    public void setQTotGer(double qTotGer) {
        this.qTotGer = qTotGer;
    }

    public double getVFor() {
        return vFor;
    }

    public void setVFor(double vFor) {
        this.vFor = vFor;
    }

    public double getVTotDed() {
        return vTotDed;
    }

    public void setVTotDed(double vTotDed) {
        this.vTotDed = vTotDed;
    }

    public double getVLiqFor() {
        return vLiqFor;
    }

    public void setVLiqFor(double vLiqFor) {
        this.vLiqFor = vLiqFor;
    }

    public List<ForDiaNFe> getForDia() {
        return forDia;
    }

    public void setForDia(List<ForDiaNFe> forDia) {
        this.forDia = forDia;
    }

    public List<DeducNFe> getDeduc() {
        return deduc;
    }

    public void setDeduc(List<DeducNFe> deduc) {
        this.deduc = deduc;
    }
}
