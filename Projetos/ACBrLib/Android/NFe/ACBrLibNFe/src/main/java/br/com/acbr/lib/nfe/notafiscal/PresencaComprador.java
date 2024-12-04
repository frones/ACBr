package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class PresencaComprador {

    private static final Map<String, PresencaComprador> lookup = new HashMap<>();

    public static final PresencaComprador pcNao = new PresencaComprador("0", "Não se aplica");
    public static final PresencaComprador pcPresencial = new PresencaComprador("1", "Presencial");
    public static final PresencaComprador pcInternet = new PresencaComprador("2", "Internet");
    public static final PresencaComprador pcTeleatendimento = new PresencaComprador("3", "Teleatendimento");
    public static final PresencaComprador pcEntregaDomicilio = new PresencaComprador("4", "Entrega a Domicílio");
    public static final PresencaComprador pcPresencialForaEstabelecimento = new PresencaComprador("5", "Presencial, Fora do Estabelecimento");
    public static final PresencaComprador pcOutros = new PresencaComprador("9", "Outros");

    private final String value;
    private final String description;

    static {
        addToLookup(pcNao);
        addToLookup(pcPresencial);
        addToLookup(pcInternet);
        addToLookup(pcTeleatendimento);
        addToLookup(pcEntregaDomicilio);
        addToLookup(pcPresencialForaEstabelecimento);
        addToLookup(pcOutros);
    }

    private PresencaComprador(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(PresencaComprador presencaComprador) {
        lookup.put(presencaComprador.value, presencaComprador);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static PresencaComprador fromValue(String value) {
        PresencaComprador presencaComprador = lookup.get(value);
        if (presencaComprador == null) {
            throw new IllegalArgumentException("Presença do Comprador inválida: " + value);
        }
        return presencaComprador;
    }

    @Override
    public String toString() {
        return value;
    }
}
