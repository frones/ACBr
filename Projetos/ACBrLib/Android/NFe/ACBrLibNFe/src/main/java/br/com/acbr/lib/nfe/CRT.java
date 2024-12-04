package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

import br.com.acbr.lib.nfe.notafiscal.IndicadorPagamento;

public class CRT {

    private static final Map<String, CRT> lookup = new HashMap<>();

    public static final CRT crtSimplesNacional = new CRT("1", "Simples Nacional");
    public static final CRT crtSimplesExcessoReceita = new CRT("2", "Simples Excesso Receita");
    public static final CRT crtRegimeNormal = new CRT("3", "Regime Normal");
    public static final CRT crtMEI = new CRT("4", "MEI");

    private final String value;
    private final String description;

    static {
        addToLookup(crtSimplesNacional);
        addToLookup(crtSimplesExcessoReceita);
        addToLookup(crtRegimeNormal);
        addToLookup(crtMEI);
    }

    private CRT(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(CRT crt) {
        lookup.put(crt.value, crt);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static CRT fromValue(String value) {
        CRT crt = lookup.get(value);
        if (crt == null) {
            throw new IllegalArgumentException("CRT inválido: " + value);
        }
        return crt;
    }

    @Override
    public String toString() {
        return value;
    }
}
