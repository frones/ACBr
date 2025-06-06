package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class CSTIS {

    private static final Map<String, CSTIS> lookup = new HashMap<>();

    public static final CSTIS cstisNenhum = new CSTIS("", "cstisNenhum");
    public static final CSTIS cstis000 = new CSTIS("000", "cstis000");

    private final String value;
    private final String description;

    static {
        addToLookup(cstisNenhum);
        addToLookup(cstis000);
    }

    private CSTIS(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSTIS cstis) {
        lookup.put(cstis.value, cstis);
    }

    public String getValue(){
        return value;
    }

    public String getDescription(){
        return description;
    }

    public static CSTIS fromValue(String value){
        CSTIS cstis = lookup.get(value);
        if(cstis == null) {
            throw new IllegalArgumentException("CSTIS inválido: " + value);
        }
        return cstis;
    }

    @Override
    public String toString() {
        return value;
    }
}
