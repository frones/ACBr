package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class ModeloDF {

    private static final Map<String, ModeloDF> lookup = new HashMap<>();
    private static final ModeloDF[] allValues;

    public static final ModeloDF moNFe = new ModeloDF("55", "NFe - Nota Fiscal Eletrônica");
    public static final ModeloDF moNFCe = new ModeloDF("65", "NFCe - Nota Fiscal do Consumidor Eletrônica");

    private final String value;
    private final String description;

    static {
        addToLookup(moNFe);
        addToLookup(moNFCe);
        allValues = new ModeloDF[] { moNFe, moNFCe };
    }

    private ModeloDF(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(ModeloDF modeloDF) {
        lookup.put(modeloDF.value, modeloDF);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static ModeloDF fromValue(String value) {
        ModeloDF modeloDF = lookup.get(value);
        if (modeloDF == null) {
            throw new IllegalArgumentException("Modelo Documento Fiscal inválido: " + value);
        }
        return modeloDF;
    }

    public static ModeloDF[] values() {
        return allValues.clone();
    }

    @Override
    public String toString() {
        return value;
    }
}
