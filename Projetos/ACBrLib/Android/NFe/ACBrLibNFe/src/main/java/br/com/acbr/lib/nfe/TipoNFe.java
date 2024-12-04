package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class TipoNFe {

    private static final Map<String, TipoNFe> lookup = new HashMap<>();

    public static final TipoNFe tnEntrada = new TipoNFe("0", "Entrada");
    public static final TipoNFe tnSaida = new TipoNFe("1", "Saída");

    private final String value;
    private final String description;

    static {
        addToLookup(tnEntrada);
        addToLookup(tnSaida);
    }

    private TipoNFe(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoNFe tipoNFe) {
        lookup.put(tipoNFe.value, tipoNFe);
    }

    public String getValue(){
        return value;
    }

    public String getDescription(){
        return description;
    }

    public static TipoNFe fromValue(String value){
        TipoNFe tipoNFe = lookup.get(value);
        if (tipoNFe == null) {
            throw new IllegalArgumentException("Tipo de Operação inválido: " + value);
        }
        return tipoNFe;
    }

    @Override
    public String toString() {
        return value;
    }
}
