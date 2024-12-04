package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class CondicaoVeiculo {

    private static final Map<String, CondicaoVeiculo> lookup = new HashMap<>();

    public static final CondicaoVeiculo cvAcabado = new CondicaoVeiculo("1", "Acabado");
    public static final CondicaoVeiculo cvInacabado = new CondicaoVeiculo("2", "Inacabado");
    public static final CondicaoVeiculo cvSemiAcabado = new CondicaoVeiculo("3", "Semi-Acabado");

    private final String value;
    private final String description;

    static {
        addToLookup(cvAcabado);
        addToLookup(cvInacabado);
        addToLookup(cvSemiAcabado);
    }

    private CondicaoVeiculo(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CondicaoVeiculo cond) {
        lookup.put(cond.value, cond);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CondicaoVeiculo fromValue(String value) {
        CondicaoVeiculo cond = lookup.get(value);
        if (cond == null) {
            throw new IllegalArgumentException("Condição do Veículo inválido: " + value);
        }
        return cond;
    }

    @Override
    public String toString() {
        return value;
    }
}
