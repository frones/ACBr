package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class TipoClassTribIS {

    private static final Map<String, TipoClassTribIS> lookup = new HashMap<>();

    public static final TipoClassTribIS ctisNenhum = new TipoClassTribIS("", "ctisNenhum");
    public static final TipoClassTribIS ctis000001 = new TipoClassTribIS("000001", "ctis000001");

    private final String value;
    private final String description;

    static {
        addToLookup(ctisNenhum);
        addToLookup(ctis000001);
    }

    private TipoClassTribIS(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoClassTribIS tipo) {
        lookup.put(tipo.value, tipo);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoClassTribIS fromValue(String value) {
        TipoClassTribIS tipo = lookup.get(value);
        if (tipo == null) {
            throw new IllegalArgumentException("Tipo de Classe de Tributável de IS inválido: " + value);
        }
        return tipo;
    }

    @Override
    public String toString() {
        return value;
    }
}
