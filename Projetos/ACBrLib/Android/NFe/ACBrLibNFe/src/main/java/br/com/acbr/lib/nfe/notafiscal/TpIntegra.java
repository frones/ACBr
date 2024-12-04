package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class TpIntegra {

    private static final Map<String, TpIntegra> lookup = new HashMap<>();

    public static final TpIntegra tiNaoInformado = new TpIntegra("", "Não Informado");
    public static final TpIntegra tiPagIntegrado = new TpIntegra("1", "Pagamento Integrado");
    public static final TpIntegra tiPagNaoIntegrado = new TpIntegra("2", "Pagamento Não Integrado");

    private final String value;
    private final String description;

    static {
        addToLookup(tiNaoInformado);
        addToLookup(tiPagIntegrado);
        addToLookup(tiPagNaoIntegrado);
    }

    private TpIntegra(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TpIntegra tpIntegra) {
        lookup.put(tpIntegra.value, tpIntegra);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TpIntegra fromValue(String value) {
        TpIntegra tpIntegra = lookup.get(value);
        if(tpIntegra == null){
            throw new IllegalArgumentException("Tipo de Integração inválido: " + value);
        } else {
            return tpIntegra;
        }
    }

    @Override
    public String toString() {
        return value;
    }
}
