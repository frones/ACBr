package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndicadorPagamento {

    private static final Map<String, IndicadorPagamento> lookup = new HashMap<>();

    public static final IndicadorPagamento ipNenhum = new IndicadorPagamento("", "Nenhum");
    public static final IndicadorPagamento ipVista = new IndicadorPagamento("0", "À Vista");
    public static final IndicadorPagamento ipPrazo = new IndicadorPagamento("1", "A Prazo");
    public static final IndicadorPagamento ipOutras = new IndicadorPagamento("2", "Outras");

    private final String value;
    private final String description;

    static {
        addToLookup(ipNenhum);
        addToLookup(ipVista);
        addToLookup(ipPrazo);
        addToLookup(ipOutras);
    }

    private IndicadorPagamento(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndicadorPagamento indicador) {
        lookup.put(indicador.value, indicador);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndicadorPagamento fromValue(String value) {
        IndicadorPagamento indicador = lookup.get(value);
        if (indicador == null) {
            throw new IllegalArgumentException("Indicador de Pagamento inválido: " + value);
        }
        return indicador;
    }

    @Override
    public String toString() {
        return value;
    }
}
