package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class RegTribISSQN {

    private static final Map<String, RegTribISSQN> lookup = new HashMap<>();

    public static final RegTribISSQN RTISSNenhum = new RegTribISSQN("0", "Nenhum");
    public static final RegTribISSQN RTISSMicroempresaMunicipal = new RegTribISSQN("1", "Microempresa Municipal");
    public static final RegTribISSQN RTISSEstimativa = new RegTribISSQN("2", "Estimativa");
    public static final RegTribISSQN RTISSSociedadeProfissionais = new RegTribISSQN("3", "Sociedade Profissionais");
    public static final RegTribISSQN RTISSCooperativa = new RegTribISSQN("4", "Cooperativa");
    public static final RegTribISSQN RTISSMEI = new RegTribISSQN("5", "MEI");
    public static final RegTribISSQN RTISSMEEPP = new RegTribISSQN("6", "ME/EPP");

    private final String value;
    private final String description;

    static{
        addToLookup(RTISSNenhum);
        addToLookup(RTISSMicroempresaMunicipal);
        addToLookup(RTISSEstimativa);
        addToLookup(RTISSSociedadeProfissionais);
        addToLookup(RTISSCooperativa);
        addToLookup(RTISSMEI);
        addToLookup(RTISSMEEPP);
    }

    private RegTribISSQN(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(RegTribISSQN regTribISSQN) {
        lookup.put(regTribISSQN.value, regTribISSQN);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static RegTribISSQN fromValue(String value) {
        RegTribISSQN regTribISSQN = lookup.get(value);
        if (regTribISSQN == null) {
            throw new IllegalArgumentException("RegTribISSQN inválido: " + value);
        }
        return regTribISSQN;
    }

    @Override
    public String toString() {
        return value;
    }
}
