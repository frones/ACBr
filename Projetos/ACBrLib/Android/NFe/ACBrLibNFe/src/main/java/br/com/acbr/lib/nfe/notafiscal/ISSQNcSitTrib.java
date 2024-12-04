package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class ISSQNcSitTrib {

    private static final Map<String, ISSQNcSitTrib> lookup = new HashMap<>();

    public static final ISSQNcSitTrib ISSQNcSitTribVazio = new ISSQNcSitTrib("", "Vazio");
    public static final ISSQNcSitTrib ISSQNcSitTribNORMAL = new ISSQNcSitTrib("N", "Normal");
    public static final ISSQNcSitTrib ISSQNcSitTribRETIDA = new ISSQNcSitTrib("R", "Retenção");
    public static final ISSQNcSitTrib ISSQNcSitTribSUBSTITUTA = new ISSQNcSitTrib("S", "Substituição");
    public static final ISSQNcSitTrib ISSQNcSitTribISENTA = new ISSQNcSitTrib("I", "Isenta");

    private final String value;
    private final String description;

    static {
        addToLookup(ISSQNcSitTribVazio);
        addToLookup(ISSQNcSitTribNORMAL);
        addToLookup(ISSQNcSitTribRETIDA);
        addToLookup(ISSQNcSitTribSUBSTITUTA);
        addToLookup(ISSQNcSitTribISENTA);
    }

    private ISSQNcSitTrib(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(ISSQNcSitTrib issqn) {
        lookup.put(issqn.value, issqn);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static ISSQNcSitTrib fromValue(String value) {
        ISSQNcSitTrib issqn = lookup.get(value);
        if (issqn == null) {
            throw new IllegalArgumentException("ISSQNcSitTrib inválido: " + value);
        }
        return issqn;
    }

    @Override
    public String toString() {
        return value;
    }
}
