package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class CSOSNIcms {

    private static final Map<String, CSOSNIcms> lookup = new HashMap<>();

    public static final CSOSNIcms csosnVazio = new CSOSNIcms("", "CSOSN Vazio");
    public static final CSOSNIcms csosn101 = new CSOSNIcms("101", "CSOSN 101");
    public static final CSOSNIcms csosn102 = new CSOSNIcms("102", "CSOSN 102");
    public static final CSOSNIcms csosn103 = new CSOSNIcms("103", "CSOSN 103");
    public static final CSOSNIcms csosn201 = new CSOSNIcms("201", "CSOSN 201");
    public static final CSOSNIcms csosn202 = new CSOSNIcms("202", "CSOSN 202");
    public static final CSOSNIcms csosn203 = new CSOSNIcms("203", "CSOSN 203");
    public static final CSOSNIcms csosn300 = new CSOSNIcms("300", "CSOSN 300");
    public static final CSOSNIcms csosn400 = new CSOSNIcms("400", "CSOSN 400");
    public static final CSOSNIcms csosn500 = new CSOSNIcms("500", "CSOSN 500");
    public static final CSOSNIcms csosn900 = new CSOSNIcms("900", "CSOSN 900");

    private final String value;
    private final String description;

    static {
        addToLookup(csosnVazio);
        addToLookup(csosn101);
        addToLookup(csosn102);
        addToLookup(csosn103);
        addToLookup(csosn201);
        addToLookup(csosn202);
        addToLookup(csosn203);
        addToLookup(csosn300);
        addToLookup(csosn400);
        addToLookup(csosn500);
        addToLookup(csosn900);
    }

    private CSOSNIcms(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSOSNIcms csosnIcms) {
        lookup.put(csosnIcms.value, csosnIcms);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CSOSNIcms fromValue(String value) {
        CSOSNIcms csosnIcms = lookup.get(value);
        if (csosnIcms == null) {
            throw new IllegalArgumentException("CSOSN ICMS inválido: " + value);
        }
        return csosnIcms;
    }

    @Override
    public String toString() {
        return value;
    }
}
