package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndBemMovelUsado {

    private static final Map<String, IndBemMovelUsado> map = new HashMap<>();

    public static final IndBemMovelUsado tieNenhum = new IndBemMovelUsado("", "tieNenhum");
    public static final IndBemMovelUsado tieSim = new IndBemMovelUsado("1", "tieSim");
    public static final IndBemMovelUsado tieNao = new IndBemMovelUsado("0", "tieNao");

    private final String value;
    private final String description;

    static {
        addToLookup(tieNenhum);
        addToLookup(tieSim);
        addToLookup(tieNao);
    }

    private IndBemMovelUsado(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndBemMovelUsado indBemMovelUsado) {
        map.put(indBemMovelUsado.value, indBemMovelUsado);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static IndBemMovelUsado fromValue(String value) {
        IndBemMovelUsado indBemMovelUsado = map.get(value);
        if (indBemMovelUsado == null) {
            throw new IllegalArgumentException("IndBemMovelUsado inválido: " + value);
        }
        return indBemMovelUsado;
    }

    @Override
    public String toString() {
        return value;
    }
}
