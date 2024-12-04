package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndicadorProcesso {

    private static final Map<String, IndicadorProcesso> lookup = new HashMap<>();

    public static final IndicadorProcesso ipSEFAZ = new IndicadorProcesso("0", "SEFAZ");
    public static final IndicadorProcesso ipJusticaFederal = new IndicadorProcesso("1", "Justica Federal");
    public static final IndicadorProcesso ipJusticaEstadual = new IndicadorProcesso("2", "Justica Estadual");
    public static final IndicadorProcesso ipSecexRFB = new IndicadorProcesso("3", "Secex RFB");
    public static final IndicadorProcesso ipOutros = new IndicadorProcesso("9", "Outros");

    private final String value;
    private final String description;

    static {
        addToLookup(ipSEFAZ);
        addToLookup(ipJusticaFederal);
        addToLookup(ipJusticaEstadual);
        addToLookup(ipSecexRFB);
        addToLookup(ipOutros);
    }

    private IndicadorProcesso(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndicadorProcesso indicador) {
        lookup.put(indicador.value, indicador);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndicadorProcesso fromValue(String value) {
        IndicadorProcesso indicador = lookup.get(value);
        if (indicador == null) {
            throw new IllegalArgumentException("Indicador de Processo inválido: " + value);
        }
        return indicador;
    }

    @Override
    public String toString() {
        return value;
    }
}
