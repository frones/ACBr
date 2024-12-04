package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class FinalidadeNFe {

    private static final Map<String, FinalidadeNFe> lookup = new HashMap<>();

    public static final FinalidadeNFe fnNormal = new FinalidadeNFe("1", "Normal");
    public static final FinalidadeNFe fnComplementar = new FinalidadeNFe("2", "Complementar");
    public static final FinalidadeNFe fnAjuste = new FinalidadeNFe("3", "Ajuste");
    public static final FinalidadeNFe fnDevolucao = new FinalidadeNFe("4", "Devolução");

    private final String value;
    private final String description;

    static {
        addToLookup(fnNormal);
        addToLookup(fnComplementar);
        addToLookup(fnAjuste);
        addToLookup(fnDevolucao);
    }

    private FinalidadeNFe(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(FinalidadeNFe finalidade) {
        lookup.put(finalidade.value, finalidade);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static FinalidadeNFe fromValue(String value) {
        FinalidadeNFe finalidade = lookup.get(value);
        if (finalidade == null) {
            throw new IllegalArgumentException("Finalidade inválida: " + value);
        }
        return finalidade;
    }

    @Override
    public String toString() {
        return value;
    }
}
