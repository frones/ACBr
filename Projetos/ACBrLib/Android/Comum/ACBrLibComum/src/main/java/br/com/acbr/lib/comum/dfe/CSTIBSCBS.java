package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class CSTIBSCBS {

    private static final Map<String, CSTIBSCBS> lookup = new HashMap<>();

    public static final CSTIBSCBS cstVazio = new CSTIBSCBS("", "CST Vazio");
    public static final CSTIBSCBS cst000 = new CSTIBSCBS("000", "CST 000");
    public static final CSTIBSCBS cst010 = new CSTIBSCBS("010", "CST 010");
    public static final CSTIBSCBS cst011 = new CSTIBSCBS("011", "CST 011");
    public static final CSTIBSCBS cst200 = new CSTIBSCBS("200", "CST 200");
    public static final CSTIBSCBS cst210 = new CSTIBSCBS("210", "CST 210");
    public static final CSTIBSCBS cst220 = new CSTIBSCBS("220", "CST 220");
    public static final CSTIBSCBS cst221 = new CSTIBSCBS("221", "CST 221");
    public static final CSTIBSCBS cst400 = new CSTIBSCBS("400", "CST 400");
    public static final CSTIBSCBS cst410 = new CSTIBSCBS("410", "CST 410");
    public static final CSTIBSCBS cst510 = new CSTIBSCBS("510", "CST 510");
    public static final CSTIBSCBS cst550 = new CSTIBSCBS("550", "CST 550");
    public static final CSTIBSCBS cst620 = new CSTIBSCBS("620", "CST 620");
    public static final CSTIBSCBS cst800 = new CSTIBSCBS("800", "CST 800");
    public static final CSTIBSCBS cst810 = new CSTIBSCBS("810", "CST 810");
    public static final CSTIBSCBS cst820 = new CSTIBSCBS("820", "CST 820");

    private final String value;
    private final String description;

    static {
        addToLookup(cstVazio);
        addToLookup(cst000);
        addToLookup(cst010);
        addToLookup(cst011);
        addToLookup(cst200);
        addToLookup(cst210);
        addToLookup(cst220);
        addToLookup(cst221);
        addToLookup(cst400);
        addToLookup(cst410);
        addToLookup(cst510);
        addToLookup(cst550);
        addToLookup(cst620);
        addToLookup(cst800);
        addToLookup(cst810);
        addToLookup(cst820);
    }

    private CSTIBSCBS(String value, String description){
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSTIBSCBS cstibscbs) {
        lookup.put(cstibscbs.value, cstibscbs);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CSTIBSCBS fromValue(String value) {
        CSTIBSCBS cstibscbs = lookup.get(value);
        if (cstibscbs == null) {
            throw new IllegalArgumentException("CSTIBSCBS inválido: " + value);
        }
        return cstibscbs;
    }

    @Override
    public String toString() {
        return value;
    }
}
