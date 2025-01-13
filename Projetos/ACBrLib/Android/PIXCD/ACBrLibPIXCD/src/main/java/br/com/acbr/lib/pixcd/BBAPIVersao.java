package br.com.acbr.lib.pixcd;

import java.util.HashMap;
import java.util.Map;

public class BBAPIVersao {

    private static final Map<String, BBAPIVersao> lookup = new HashMap<>();
    private static final BBAPIVersao[] allValues;

    public static final BBAPIVersao apiVersao1 = new BBAPIVersao("0", "API Versão 1");
    public static final BBAPIVersao apiVersao2 = new BBAPIVersao("1", "API Versão 2");

    private final String value;
    private final String description;

    static{
        addToLookup(apiVersao1);
        addToLookup(apiVersao2);
        allValues = new BBAPIVersao[]{apiVersao1, apiVersao2};
    }

    private BBAPIVersao(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(BBAPIVersao bbapiVersao) {
        lookup.put(bbapiVersao.value, bbapiVersao);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static BBAPIVersao fromValue(String value){
        BBAPIVersao bbapiVersao = lookup.get(value);
        if (bbapiVersao == null){
            throw new IllegalArgumentException("Versão BB API inválido: " + value);
        }
        return bbapiVersao;
    }

    public static BBAPIVersao[] values(){
        return allValues.clone();
    }

    @Override
    public String toString(){
        return value;
    }
}
