package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class DestinoOperacao {

    private static final Map<String, DestinoOperacao> lookup = new HashMap<>();

    public static final DestinoOperacao doInterna = new DestinoOperacao("1", "Interna");
    public static final DestinoOperacao doInterestadual = new DestinoOperacao("2", "Interestadual");
    public static final DestinoOperacao doExterior = new DestinoOperacao("3", "Exterior");

    private final String value;
    private final String description;

    static {
        addToLookup(doInterna);
        addToLookup(doInterestadual);
        addToLookup(doExterior);
    }

    private DestinoOperacao(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(DestinoOperacao destino) {
        lookup.put(destino.value, destino);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static DestinoOperacao fromValue(String value) {
        DestinoOperacao destino = lookup.get(value);
        if (destino == null) {
            throw new IllegalArgumentException("Destino de Operação inválido: " + value);
        }
        return destino;
    }
    @Override
    public String toString() {
        return value;
    }
}
