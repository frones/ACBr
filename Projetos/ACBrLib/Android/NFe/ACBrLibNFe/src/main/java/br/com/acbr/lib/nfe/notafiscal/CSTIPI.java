package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class CSTIPI {

    private static final Map<String, CSTIPI> lookup = new HashMap<>();

    public static final CSTIPI ipi00 = new CSTIPI("00", "IPI 00");
    public static final CSTIPI ipi49 = new CSTIPI("49", "IPI 49");
    public static final CSTIPI ipi50 = new CSTIPI("50", "IPI 50");
    public static final CSTIPI ipi99 = new CSTIPI("99", "IPI 99");
    public static final CSTIPI ipi01 = new CSTIPI("01", "IPI 01");
    public static final CSTIPI ipi02 = new CSTIPI("02", "IPI 02");
    public static final CSTIPI ipi03 = new CSTIPI("03", "IPI 03");
    public static final CSTIPI ipi04 = new CSTIPI("04", "IPI 04");
    public static final CSTIPI ipi05 = new CSTIPI("05", "IPI 05");
    public static final CSTIPI ipi51 = new CSTIPI("51", "IPI 51");
    public static final CSTIPI ipi52 = new CSTIPI("52", "IPI 52");
    public static final CSTIPI ipi53 = new CSTIPI("53", "IPI 53");
    public static final CSTIPI ipi54 = new CSTIPI("54", "IPI 54");
    public static final CSTIPI ipi55 = new CSTIPI("55", "IPI 55");

    private final String value;
    private final String description;

    static {
        addToLookup(ipi00);
        addToLookup(ipi49);
        addToLookup(ipi50);
        addToLookup(ipi99);
        addToLookup(ipi01);
        addToLookup(ipi02);
        addToLookup(ipi03);
        addToLookup(ipi04);
        addToLookup(ipi05);
        addToLookup(ipi51);
        addToLookup(ipi52);
        addToLookup(ipi53);
        addToLookup(ipi54);
        addToLookup(ipi55);
    }

    private CSTIPI(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSTIPI cst) {
        lookup.put(cst.value, cst);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CSTIPI fromValue(String value) {
        CSTIPI cst = lookup.get(value);
        if (cst == null) {
            throw new IllegalArgumentException("CSTIPI inválido: " + value);
        }
        return cst;
    }

    @Override
    public String toString() {
        return value;
    }
}
