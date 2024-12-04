package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class CSTPIS {

    private static final Map<String, CSTPIS> lookup = new HashMap<>();

    public static final CSTPIS pis01 = new CSTPIS("01", "PIS 01");
    public static final CSTPIS pis02 = new CSTPIS("02", "PIS 02");
    public static final CSTPIS pis03 = new CSTPIS("03", "PIS 03");
    public static final CSTPIS pis04 = new CSTPIS("04", "PIS 04");
    public static final CSTPIS pis05 = new CSTPIS("05", "PIS 05");
    public static final CSTPIS pis06 = new CSTPIS("06", "PIS 06");
    public static final CSTPIS pis07 = new CSTPIS("07", "PIS 07");
    public static final CSTPIS pis08 = new CSTPIS("08", "PIS 08");
    public static final CSTPIS pis09 = new CSTPIS("09", "PIS 09");
    public static final CSTPIS pis49 = new CSTPIS("49", "PIS 49");
    public static final CSTPIS pis50 = new CSTPIS("50", "PIS 50");
    public static final CSTPIS pis51 = new CSTPIS("51", "PIS 51");
    public static final CSTPIS pis52 = new CSTPIS("52", "PIS 52");
    public static final CSTPIS pis53 = new CSTPIS("53", "PIS 53");
    public static final CSTPIS pis54 = new CSTPIS("54", "PIS 54");
    public static final CSTPIS pis55 = new CSTPIS("55", "PIS 55");
    public static final CSTPIS pis56 = new CSTPIS("56", "PIS 56");
    public static final CSTPIS pis60 = new CSTPIS("60", "PIS 60");
    public static final CSTPIS pis61 = new CSTPIS("61", "PIS 61");
    public static final CSTPIS pis62 = new CSTPIS("62", "PIS 62");
    public static final CSTPIS pis63 = new CSTPIS("63", "PIS 63");
    public static final CSTPIS pis64 = new CSTPIS("64", "PIS 64");
    public static final CSTPIS pis65 = new CSTPIS("65", "PIS 65");
    public static final CSTPIS pis66 = new CSTPIS("66", "PIS 66");
    public static final CSTPIS pis67 = new CSTPIS("67", "PIS 67");
    public static final CSTPIS pis70 = new CSTPIS("70", "PIS 70");
    public static final CSTPIS pis71 = new CSTPIS("71", "PIS 71");
    public static final CSTPIS pis72 = new CSTPIS("72", "PIS 72");
    public static final CSTPIS pis73 = new CSTPIS("73", "PIS 73");
    public static final CSTPIS pis74 = new CSTPIS("74", "PIS 74");
    public static final CSTPIS pis75 = new CSTPIS("75", "PIS 75");
    public static final CSTPIS pis98 = new CSTPIS("98", "PIS 98");
    public static final CSTPIS pis99 = new CSTPIS("99", "PIS 99");

    private final String value;
    private final String description;

    static {
        addToLookup(pis01);
        addToLookup(pis02);
        addToLookup(pis03);
        addToLookup(pis04);
        addToLookup(pis05);
        addToLookup(pis06);
        addToLookup(pis07);
        addToLookup(pis08);
        addToLookup(pis09);
        addToLookup(pis49);
        addToLookup(pis50);
        addToLookup(pis51);
        addToLookup(pis52);
        addToLookup(pis53);
        addToLookup(pis54);
        addToLookup(pis55);
        addToLookup(pis56);
        addToLookup(pis60);
        addToLookup(pis61);
        addToLookup(pis62);
        addToLookup(pis63);
        addToLookup(pis64);
        addToLookup(pis65);
        addToLookup(pis66);
        addToLookup(pis67);
        addToLookup(pis70);
        addToLookup(pis71);
        addToLookup(pis72);
        addToLookup(pis73);
        addToLookup(pis74);
        addToLookup(pis75);
        addToLookup(pis98);
        addToLookup(pis99);
    }

    private CSTPIS(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSTPIS cstpis){
        lookup.put(cstpis.value, cstpis);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CSTPIS fromValue(String value) {
        CSTPIS cstpis = lookup.get(value);
        if (cstpis == null) {
            throw new IllegalArgumentException("CST PIS inválido: " + value);
        }
        return cstpis;
    }

    @Override
    public String toString() {
        return value;
    }
}