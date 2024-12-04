package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class MotRedAdRem {

    private static final Map<String, MotRedAdRem> lookup = new HashMap<>();

    public static final MotRedAdRem motTranspColetivo = new MotRedAdRem("1", "Transporte coletivo");
    public static final MotRedAdRem motOutros = new MotRedAdRem("9", "Outros");

    private final String value;
    private final String description;

    static{
        addToLookup(motTranspColetivo);
        addToLookup(motOutros);
    }

    private MotRedAdRem(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(MotRedAdRem motRedAdRem) {
        lookup.put(motRedAdRem.value, motRedAdRem);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static MotRedAdRem fromValue(String value) {
        MotRedAdRem motRedAdRem = lookup.get(value);
        if (motRedAdRem == null) {
            throw new IllegalArgumentException("MotRedAdRem inválido: " + value);
        }
        return motRedAdRem;
    }

    @Override
    public String toString() {
        return value;
    }
}
