package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndEscala {

    private static final Map<String, IndEscala> lookup = new HashMap<>();

    public static final IndEscala ieNenhum = new IndEscala("");
    public static final IndEscala ieRelevante = new IndEscala("S");
    public static final IndEscala ieNaoRelevante = new IndEscala("N");

    private final String value;
    private final String description;

    static {
        addToLookup(ieNenhum);
        addToLookup(ieRelevante);
        addToLookup(ieNaoRelevante);
    }

    private IndEscala(String value) {
        this.value = value;
        this.description = value;
    }

    private static void addToLookup(IndEscala indEscala) {
        lookup.put(indEscala.value, indEscala);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndEscala fromValue(String value) {
        IndEscala indEscala = lookup.get(value);
        if (indEscala == null) {
            throw new IllegalArgumentException("Indicador de Escala Relevante inválido: " + value);
        }
        return indEscala;
    }

    @Override
    public String toString() {
        return value;
    }
}
