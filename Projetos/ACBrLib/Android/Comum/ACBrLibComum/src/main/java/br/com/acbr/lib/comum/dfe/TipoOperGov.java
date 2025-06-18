package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class TipoOperGov {

    private static final Map<String, TipoOperGov> lookup = new HashMap<>();

    public static final TipoOperGov togFornecimento = new TipoOperGov("1", "Fornecimento");
    public static final TipoOperGov togRecebimentoPag = new TipoOperGov("2", "RecebimentoPag");

    private final String value;
    private final String description;

    static {
        addToLookup(togFornecimento);
        addToLookup(togRecebimentoPag);
    }

    private TipoOperGov(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoOperGov tipoOperGov) {
        lookup.put(tipoOperGov.value, tipoOperGov);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoOperGov fromValue(String value) {
        TipoOperGov tipoOperGov = lookup.get(value);
        if (tipoOperGov == null) {
            throw new IllegalArgumentException("Tipo Operação Governamental inválido: " + value);
        }
        return tipoOperGov;
    }

    @Override
    public String toString() {
        return value;
    }
}
