package br.com.acbr.lib.nfe.notafiscal;

import java.util.ArrayList;
import java.util.List;

public class DadosAdicionaisNFe {
    private final List<InfoAdicionalNfe> obsCont = new ArrayList<>();
    private final List<InfoAdicionalNfe> obsFisco = new ArrayList<>();
    private final List<ProcRefNFe> procRef = new ArrayList<>();

    public List<InfoAdicionalNfe> getObsCont() {
        return obsCont;
    }

    public List<InfoAdicionalNfe> getObsFisco() {
        return obsFisco;
    }

    public List<ProcRefNFe> getProcRef() {
        return procRef;
    }
}
