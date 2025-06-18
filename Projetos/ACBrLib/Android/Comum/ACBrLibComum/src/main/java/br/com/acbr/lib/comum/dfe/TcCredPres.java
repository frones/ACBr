package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class TcCredPres {

    private static final Map<String, TcCredPres> lookup = new HashMap<>();

    public static final TcCredPres cpNenhum = new TcCredPres("", "cpNenhum");
    public static final TcCredPres cp01 = new TcCredPres("01", "cp01");
    public static final TcCredPres cp02 = new TcCredPres("02", "cp02");
    public static final TcCredPres cp03 = new TcCredPres("03", "cp03");
    public static final TcCredPres cp04 = new TcCredPres("04", "cp04");
    public static final TcCredPres cp05 = new TcCredPres("05", "cp05");
    public static final TcCredPres cp06 = new TcCredPres("06", "cp06");
    public static final TcCredPres cp07 = new TcCredPres("07", "cp07");
    public static final TcCredPres cp08 = new TcCredPres("08", "cp08");
    public static final TcCredPres cp09 = new TcCredPres("09", "cp09");
    public static final TcCredPres cp10 = new TcCredPres("10", "cp10");
    public static final TcCredPres cp11 = new TcCredPres("11", "cp11");
    public static final TcCredPres cp12 = new TcCredPres("12", "cp12");
    public static final TcCredPres cp13 = new TcCredPres("13", "cp13");

    private final String value;
    private final String description;

    static {
        addToLookup(cpNenhum);
        addToLookup(cp01);
        addToLookup(cp02);
        addToLookup(cp03);
        addToLookup(cp04);
        addToLookup(cp05);
        addToLookup(cp06);
        addToLookup(cp07);
        addToLookup(cp08);
        addToLookup(cp09);
        addToLookup(cp10);
        addToLookup(cp11);
        addToLookup(cp12);
        addToLookup(cp13);
    }

    private TcCredPres(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TcCredPres tcCredPres) {
        lookup.put(tcCredPres.value, tcCredPres);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TcCredPres fromValue(String value) {
        TcCredPres tcCredPres = lookup.get(value);
        if (tcCredPres == null) {
            throw new IllegalArgumentException("TcCredPres inválido: " + value);
        }
        return tcCredPres;
    }

    @Override
    public String toString() {
        return value;
    }
}
