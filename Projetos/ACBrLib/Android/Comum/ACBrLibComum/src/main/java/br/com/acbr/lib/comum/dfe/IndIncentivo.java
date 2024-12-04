package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class IndIncentivo {

    private static final Map<String, IndIncentivo> lookup = new HashMap<>();

    public static final IndIncentivo iiSim = new IndIncentivo("1", "Sim");
    public static final IndIncentivo iiNao = new IndIncentivo("2", "Não");

    private final String value;
    private final String description;

    static {
        addToLookup(iiSim);
        addToLookup(iiNao);
    }

    private IndIncentivo(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndIncentivo indIncentivo) {
        lookup.put(indIncentivo.value, indIncentivo);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndIncentivo fromValue(String value) {
        IndIncentivo indIncentivo = lookup.get(value);
        if (indIncentivo == null) {
            throw new IllegalArgumentException("IndIncentivo inválido: " + value);
        }
        return indIncentivo;
    }

    @Override
    public String toString() {
        return value;
    }
}
