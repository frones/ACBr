package br.com.acbr.lib.pixcd;

import java.util.HashMap;
import java.util.Map;

public class Ambiente {

    private static final Map<String, Ambiente> lookup = new HashMap<>();
    private static final Ambiente[] allValues;

    public static final Ambiente ambTeste = new Ambiente("0", "Teste");
    public static final Ambiente ambProducao = new Ambiente("1", "Produção");
    public static final Ambiente ambPreProducao = new Ambiente("2", "Pré-Produção");

    private final String value;
    private final String description;

    static{
        addToLookup(ambTeste);
        addToLookup(ambProducao);
        addToLookup(ambPreProducao);
        allValues = new Ambiente[]{ambTeste, ambProducao, ambPreProducao};
    }

    private Ambiente(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(Ambiente ambiente) {
        lookup.put(ambiente.value, ambiente);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static Ambiente fromValue(String value){
        Ambiente ambiente = lookup.get(value);
        if (ambiente == null){
            throw new IllegalArgumentException("Ambiente inválido: " + value);
        }
        return ambiente;
    }

    public static Ambiente[] values(){
        return allValues.clone();
    }

    @Override
    public String toString(){
        return value;
    }
}
