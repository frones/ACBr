package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndIntermed {

    private static final Map<String, IndIntermed> lookup = new HashMap<>();

    public static final IndIntermed iiSemOperacao = new IndIntermed("", "Sem Operação");
    public static final IndIntermed iiOperacaoSemIntermediador = new IndIntermed("0", "Operação Sem Intermediador");
    public static final IndIntermed iiOperacaoComIntermediador = new IndIntermed("1", "Operação Com Intermediador");

    private final String value;
    private final String description;

    static {
        addToLookup(iiSemOperacao);
        addToLookup(iiOperacaoSemIntermediador);
        addToLookup(iiOperacaoComIntermediador);
    }

    private IndIntermed(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndIntermed indIntermed) {
        lookup.put(indIntermed.value, indIntermed);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndIntermed fromValue(String value) {
        IndIntermed indIntermed = lookup.get(value);
        if (indIntermed == null) {
            throw new IllegalArgumentException("IndIntermed inválido: " + value);
        }
        return indIntermed;
    }

    @Override
    public String toString() {
        return value;
    }
}
