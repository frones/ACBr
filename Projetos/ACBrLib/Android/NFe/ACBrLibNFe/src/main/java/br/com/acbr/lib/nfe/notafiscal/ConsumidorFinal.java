package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class ConsumidorFinal {

    private static final Map<String, ConsumidorFinal> lookup = new HashMap<>();

    public static final ConsumidorFinal cfNao = new ConsumidorFinal("0", "Não Consumidor Final");
    public static final ConsumidorFinal cfConsumidorFinal = new ConsumidorFinal("1", "Consumidor Final");

    private final String value;
    private final String description;

    static {
        addToLookup(cfNao);
        addToLookup(cfConsumidorFinal);
    }

    private ConsumidorFinal(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(ConsumidorFinal consumidorFinal) {
        lookup.put(consumidorFinal.value, consumidorFinal);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static ConsumidorFinal fromValue(String value) {
        ConsumidorFinal consumidorFinal = lookup.get(value);
        if (consumidorFinal == null) {
            throw new IllegalArgumentException("Consumidor Final inválido: " + value);
        }
        return consumidorFinal;
    }

    @Override
    public String toString() {
        return value;
    }
}
