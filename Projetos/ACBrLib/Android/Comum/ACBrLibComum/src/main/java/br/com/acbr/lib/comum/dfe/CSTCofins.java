package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class CSTCofins {

    private static final Map<String, CSTCofins> lookup = new HashMap<>();

    public static final CSTCofins cof01 = new CSTCofins("01", "COFINS 01");
    public static final CSTCofins cof02 = new CSTCofins("02", "COFINS 02");
    public static final CSTCofins cof03 = new CSTCofins("03", "COFINS 03");
    public static final CSTCofins cof04 = new CSTCofins("04", "COFINS 04");
    public static final CSTCofins cof05 = new CSTCofins("05", "COFINS 05");
    public static final CSTCofins cof06 = new CSTCofins("06", "COFINS 06");
    public static final CSTCofins cof07 = new CSTCofins("07", "COFINS 07");
    public static final CSTCofins cof08 = new CSTCofins("08", "COFINS 08");
    public static final CSTCofins cof09 = new CSTCofins("09", "COFINS 09");
    public static final CSTCofins cof49 = new CSTCofins("49", "COFINS 49");
    public static final CSTCofins cof50 = new CSTCofins("50", "COFINS 50");
    public static final CSTCofins cof51 = new CSTCofins("51", "COFINS 51");
    public static final CSTCofins cof52 = new CSTCofins("52", "COFINS 52");
    public static final CSTCofins cof53 = new CSTCofins("53", "COFINS 53");
    public static final CSTCofins cof54 = new CSTCofins("54", "COFINS 54");
    public static final CSTCofins cof55 = new CSTCofins("55", "COFINS 55");
    public static final CSTCofins cof56 = new CSTCofins("56", "COFINS 56");
    public static final CSTCofins cof60 = new CSTCofins("60", "COFINS 60");
    public static final CSTCofins cof61 = new CSTCofins("61", "COFINS 61");
    public static final CSTCofins cof62 = new CSTCofins("62", "COFINS 62");
    public static final CSTCofins cof63 = new CSTCofins("63", "COFINS 63");
    public static final CSTCofins cof64 = new CSTCofins("64", "COFINS 64");
    public static final CSTCofins cof65 = new CSTCofins("65", "COFINS 65");
    public static final CSTCofins cof66 = new CSTCofins("66", "COFINS 66");
    public static final CSTCofins cof67 = new CSTCofins("67", "COFINS 67");
    public static final CSTCofins cof70 = new CSTCofins("70", "COFINS 70");
    public static final CSTCofins cof71 = new CSTCofins("71", "COFINS 71");
    public static final CSTCofins cof72 = new CSTCofins("72", "COFINS 72");
    public static final CSTCofins cof73 = new CSTCofins("73", "COFINS 73");
    public static final CSTCofins cof74 = new CSTCofins("74", "COFINS 74");
    public static final CSTCofins cof75 = new CSTCofins("75", "COFINS 75");
    public static final CSTCofins cof98 = new CSTCofins("98", "COFINS 98");
    public static final CSTCofins cof99 = new CSTCofins("99", "COFINS 99");

    private final String value;
    private final String description;

    static {
        addToLookup(cof01);
        addToLookup(cof02);
        addToLookup(cof03);
        addToLookup(cof04);
        addToLookup(cof05);
        addToLookup(cof06);
        addToLookup(cof07);
        addToLookup(cof08);
        addToLookup(cof09);
        addToLookup(cof49);
        addToLookup(cof50);
        addToLookup(cof51);
        addToLookup(cof52);
        addToLookup(cof53);
        addToLookup(cof54);
        addToLookup(cof55);
        addToLookup(cof56);
        addToLookup(cof60);
        addToLookup(cof61);
        addToLookup(cof62);
        addToLookup(cof63);
        addToLookup(cof64);
        addToLookup(cof65);
        addToLookup(cof66);
        addToLookup(cof67);
        addToLookup(cof70);
        addToLookup(cof71);
        addToLookup(cof72);
        addToLookup(cof73);
        addToLookup(cof74);
        addToLookup(cof75);
        addToLookup(cof98);
        addToLookup(cof99);
    }

    private CSTCofins(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSTCofins cstCofins) {
        lookup.put(cstCofins.value, cstCofins);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CSTCofins fromValue(String value) {
        CSTCofins cstCofins = lookup.get(value);
        if (cstCofins == null) {
            throw new IllegalArgumentException("CST COFINS inválido: " + value);
        }
        return cstCofins;
    }

    @Override
    public String toString() {
        return value;
    }
}
