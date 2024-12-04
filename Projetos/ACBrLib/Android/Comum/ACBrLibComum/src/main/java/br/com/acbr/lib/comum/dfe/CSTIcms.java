package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class CSTIcms {

    private static final Map<String, CSTIcms> lookup = new HashMap<>();

    public static final CSTIcms cstVazio = new CSTIcms("", "CST Vazio");
    public static final CSTIcms cst00 = new CSTIcms("00", "CST 00");
    public static final CSTIcms cst01 = new CSTIcms("01", "CST 01");
    public static final CSTIcms cst02 = new CSTIcms("02", "CST 02");
    public static final CSTIcms cst10 = new CSTIcms("10", "CST 10");
    public static final CSTIcms cst12 = new CSTIcms("12", "CST 12");
    public static final CSTIcms cst13 = new CSTIcms("13", "CST 13");
    public static final CSTIcms cst14 = new CSTIcms("14", "CST 14");
    public static final CSTIcms cst15 = new CSTIcms("15", "CST 15");
    public static final CSTIcms cst20 = new CSTIcms("20", "CST 20");
    public static final CSTIcms cst21 = new CSTIcms("21", "CST 21");
    public static final CSTIcms cst30 = new CSTIcms("30", "CST 30");
    public static final CSTIcms cst40 = new CSTIcms("40", "CST 40");
    public static final CSTIcms cst41 = new CSTIcms("41", "CST 41");
    public static final CSTIcms cst45 = new CSTIcms("45", "CST 45");
    public static final CSTIcms cst50 = new CSTIcms("50", "CST 50");
    public static final CSTIcms cst51 = new CSTIcms("51", "CST 51");
    public static final CSTIcms cst53 = new CSTIcms("53", "CST 53");
    public static final CSTIcms cst60 = new CSTIcms("60", "CST 60");
    public static final CSTIcms cst70 = new CSTIcms("70", "CST 70");
    public static final CSTIcms cst72 = new CSTIcms("72", "CST 72");
    public static final CSTIcms cst73 = new CSTIcms("73", "CST 73");
    public static final CSTIcms cst74 = new CSTIcms("74", "CST 74");
    public static final CSTIcms cst80 = new CSTIcms("80", "CST 80");
    public static final CSTIcms cst81 = new CSTIcms("81", "CST 81");
    public static final CSTIcms cst90 = new CSTIcms("90", "CST 90");
    public static final CSTIcms cstICMSOutraUF = new CSTIcms("91", "CST 91");
    public static final CSTIcms cstICMSSN = new CSTIcms("SN", "CST SN");
    public static final CSTIcms cstPart10 = new CSTIcms("10part", "CST Part10");
    public static final CSTIcms cstPart90 = new CSTIcms("90part", "CST part90");
    public static final CSTIcms cstRep41 = new CSTIcms("41rep", "CST 41");
    public static final CSTIcms cstRep60 = new CSTIcms("60rep", "CST 60");
    public static final CSTIcms cst61 = new CSTIcms("61", "CST 61");

    private final String value;
    private final String description;

    static {
        addToLookup(cstVazio);
        addToLookup(cst00);
        addToLookup(cst01);
        addToLookup(cst02);
        addToLookup(cst10);
        addToLookup(cst12);
        addToLookup(cst13);
        addToLookup(cst14);
        addToLookup(cst15);
        addToLookup(cst20);
        addToLookup(cst21);
        addToLookup(cst30);
        addToLookup(cst40);
        addToLookup(cst41);
        addToLookup(cst45);
        addToLookup(cst50);
        addToLookup(cst51);
        addToLookup(cst53);
        addToLookup(cst60);
        addToLookup(cst70);
        addToLookup(cst72);
        addToLookup(cst73);
        addToLookup(cst74);
        addToLookup(cst80);
        addToLookup(cst81);
        addToLookup(cst90);
        addToLookup(cstICMSOutraUF);
        addToLookup(cstICMSSN);
        addToLookup(cstPart10);
        addToLookup(cstPart90);
        addToLookup(cstRep41);
        addToLookup(cstRep60);
        addToLookup(cst61);
    }

    private CSTIcms(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CSTIcms cstIcms) {
        lookup.put(cstIcms.value, cstIcms);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CSTIcms fromValue(String value) {
        CSTIcms cstIcms = lookup.get(value);
        if (cstIcms == null) {
            throw new IllegalArgumentException("CST ICMS inválido: " + value);
        }
        return cstIcms;
    }

    @Override
    public String toString() {
        return value;
    }
}
