package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class TipoArma {

    private static final Map<String, TipoArma> map = new HashMap<>();

    public static final TipoArma taUsoPermitido = new TipoArma("0", "Uso Permitido");
    public static final TipoArma taUsoRestrito = new TipoArma("1", "Uso Restrito");

    private final String value;
    private final String description;

    static {
        addToLookup(taUsoPermitido);
        addToLookup(taUsoRestrito);
    }

    private TipoArma(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoArma tipoArma) {
        map.put(tipoArma.value, tipoArma);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoArma fromValue(String value) {
        TipoArma tipoArma = map.get(value);
        if (tipoArma == null) {
            throw new IllegalArgumentException("Tipo de Arma inválido: " + value);
        }
        return tipoArma;
    }

    @Override
    public String toString() {
        return value;
    }
}
