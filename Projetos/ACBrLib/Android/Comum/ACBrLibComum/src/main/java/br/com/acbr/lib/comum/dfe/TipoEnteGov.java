package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class TipoEnteGov {

    private static final Map<String, TipoEnteGov> lookup = new HashMap<>();

    public static final TipoEnteGov tcgUniao = new TipoEnteGov("1", "União");
    public static final TipoEnteGov tcgEstados = new TipoEnteGov("2", "Estados");
    public static final TipoEnteGov tcgDistritoFederal = new TipoEnteGov("3", "Distrito Federal");
    public static final TipoEnteGov tcgMunicipios = new TipoEnteGov("4", "Municipios");

    private final String value;
    private final String description;

    static {
        addToLookup(tcgUniao);
        addToLookup(tcgEstados);
        addToLookup(tcgDistritoFederal);
        addToLookup(tcgMunicipios);
    }

    private TipoEnteGov(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoEnteGov tipoEnteGov) {
        lookup.put(tipoEnteGov.value, tipoEnteGov);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoEnteGov fromValue(String value) {
        TipoEnteGov tipoEnteGov = lookup.get(value);
        if (tipoEnteGov == null) {
            throw new IllegalArgumentException("Tipo Entidade Governamental inválido: " + value);
        }
        return tipoEnteGov;
    }

    @Override
    public String toString() {
        return value;
    }
}