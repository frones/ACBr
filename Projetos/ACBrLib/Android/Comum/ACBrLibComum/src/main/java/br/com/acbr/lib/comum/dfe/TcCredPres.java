package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class TcCredPres {

    private static final Map<String, TcCredPres> lookup = new HashMap<>();

    public static final TcCredPres cpNenhum = new TcCredPres("", "cpNenhum");
    public static final TcCredPres cp00 = new TcCredPres("00", "cp00");

    private final String value;
    private final String description;

    static {
        addToLookup(cpNenhum);
        addToLookup(cp00);
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
