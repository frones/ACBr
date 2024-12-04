package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class TipoAmbiente {

    private static final Map<String, TipoAmbiente> lookup = new HashMap<>();

    public static final TipoAmbiente taProducao = new TipoAmbiente("1", "Produção");
    public static final TipoAmbiente taHomologacao = new TipoAmbiente("2", "Homologação");

    private final String value;
    private final String description;

    static {
        addToLookup(taProducao);
        addToLookup(taHomologacao);
    }

    private TipoAmbiente(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoAmbiente tipoAmbiente) {
        lookup.put(tipoAmbiente.value, tipoAmbiente);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoAmbiente fromValue(String value) {
        TipoAmbiente tipoAmbiente = lookup.get(value);
        if (tipoAmbiente == null) {
            throw new IllegalArgumentException("Tipo Ambiente inválido: " + value);
        }
        return tipoAmbiente;
    }

    @Override
    public String toString() {
        return value;
    }
}
