package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class IndicadorIE {

    private static final Map<String, IndicadorIE> lookup = new HashMap<>();

    public static final IndicadorIE inContribuinte = new IndicadorIE("1", "Contribuinte");
    public static final IndicadorIE inIsento = new IndicadorIE("2", "Isento");
    public static final IndicadorIE inNaoContribuinte = new IndicadorIE("9", "Não Contribuinte");

    private final String value;
    private final String description;

    static {
        addToLookup(inContribuinte);
        addToLookup(inIsento);
        addToLookup(inNaoContribuinte);
    }

    private IndicadorIE(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndicadorIE indicadorIE) {
        lookup.put(indicadorIE.value, indicadorIE);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndicadorIE fromValue(String value) {
        IndicadorIE indicadorIE = lookup.get(value);
        if (indicadorIE == null) {
            throw new IllegalArgumentException("Indicador IE inválido: " + value);
        }
        return indicadorIE;
    }

    @Override
    public String toString() {
        return value;
    }
}
