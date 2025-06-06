package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class TipoNFeCredito {

    private static final Map<String, TipoNFeCredito> lookup = new HashMap<>();

    public static final TipoNFeCredito tcNenhum = new TipoNFeCredito("", "Nenhum");
    public static final TipoNFeCredito tc01 = new TipoNFeCredito("01", "Crédito");

    private final String value;
    private final String description;

    static {
        addToLookup(tcNenhum);
        addToLookup(tc01);
    }

    private TipoNFeCredito(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoNFeCredito tipoNFeCredito) {
        lookup.put(tipoNFeCredito.value, tipoNFeCredito);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoNFeCredito fromValue(String value) {
        TipoNFeCredito tipoNFeCredito = lookup.get(value);
        if (tipoNFeCredito == null) {
            throw new IllegalArgumentException("TipoNFeCredito inválido: " + value);
        }
        return tipoNFeCredito;
    }

    @Override
    public String toString() {
        return value;
    }
}
