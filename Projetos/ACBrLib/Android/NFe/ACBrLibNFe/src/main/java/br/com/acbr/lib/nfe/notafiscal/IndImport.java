package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndImport {

    private static final Map<String, IndImport> lookup = new HashMap<>();

    public static final IndImport Nacional = new IndImport("0", "Nacional");
    public static final IndImport Importado = new IndImport("1", "Importado");

    private final String value;
    private final String description;

    static{
        addToLookup(Nacional);
        addToLookup(Importado);
    }

    private IndImport(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndImport indImport) {
        lookup.put(indImport.value, indImport);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndImport fromValue(String value) {
        IndImport indImport = lookup.get(value);
        if (indImport == null) {
            throw new IllegalArgumentException("IndImport inválido: " + value);
        }
        return indImport;
    }

    @Override
    public String toString() {
        return value;
    }
}
