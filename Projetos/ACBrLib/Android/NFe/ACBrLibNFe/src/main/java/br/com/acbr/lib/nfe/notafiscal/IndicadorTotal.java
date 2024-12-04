package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndicadorTotal {

    private static final Map<String, IndicadorTotal> lookup = new HashMap<>();

    public static final IndicadorTotal itNaoSomaTotalNFe = new IndicadorTotal("0", "Não Soma Total NFe");
    public static final IndicadorTotal itSomaTotalNFe = new IndicadorTotal("1", "Soma Total NFe");

    private final String value;
    private final String description;

    static {
        addToLookup(itNaoSomaTotalNFe);
        addToLookup(itSomaTotalNFe);
    }

    private IndicadorTotal(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndicadorTotal indicador) {
        lookup.put(indicador.value, indicador);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndicadorTotal fromValue(String value) {
        IndicadorTotal indicador = lookup.get(value);
        if (indicador == null) {
            throw new IllegalArgumentException("Indicador Total inválido: " + value);
        }
        return indicador;
    }

    @Override
    public String toString() {
        return value;
    }
}
