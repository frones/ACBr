package br.com.acbr.lib.pixcd;

import java.util.HashMap;
import java.util.Map;

public class TipoChave {

    private static final Map<String, TipoChave> lookup = new HashMap<>();
    private static final TipoChave[] allValues;

    public static final TipoChave tchNenhuma = new TipoChave("0", "Nenhuma");
    public static final TipoChave tchEmail = new TipoChave("1", "Email");
    public static final TipoChave tchCPF = new TipoChave("2", "CPF");
    public static final TipoChave tchCNPJ = new TipoChave("3", "CNPJ");
    public static final TipoChave tchCelular = new TipoChave("4", "Celular");
    public static final TipoChave tchAleatoria = new TipoChave("5", "Aleatória");

    private final String value;
    private final String description;

    static{
        addToLookup(tchNenhuma);
        addToLookup(tchEmail);
        addToLookup(tchCPF);
        addToLookup(tchCNPJ);
        addToLookup(tchCelular);
        addToLookup(tchAleatoria);
        allValues = new TipoChave[]{tchNenhuma, tchEmail, tchCPF, tchCNPJ, tchCelular, tchAleatoria};
    }

    private TipoChave(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoChave tipoChave) {
        lookup.put(tipoChave.value, tipoChave);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoChave fromValue(String value){
        TipoChave tipoChave = lookup.get(value);
        if (tipoChave == null){
            throw new IllegalArgumentException("Tipo de Chave inválida: " + value);
        }
        return tipoChave;
    }

    public static TipoChave[] values(){
        return allValues.clone();
    }

    @Override
    public String toString(){
        return value;
    }
}
