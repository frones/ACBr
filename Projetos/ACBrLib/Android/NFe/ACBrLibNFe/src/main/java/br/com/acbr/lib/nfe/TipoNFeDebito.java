package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class TipoNFeDebito {

    private static final Map<String, TipoNFeDebito> lookup = new HashMap<>();

    public static final TipoNFeDebito tdNenhum = new TipoNFeDebito("", "Nenhum");
    public static final TipoNFeDebito tdTransferenciaCreditoCooperativa = new TipoNFeDebito("01", "Transferência Crédito Cooperativa");
    public static final TipoNFeDebito tdAnulacao = new TipoNFeDebito("02", "Anulação");
    public static final TipoNFeDebito tdDebitosNaoProcessadas = new TipoNFeDebito("03", "Débitos Não Processadas");
    public static final TipoNFeDebito tdMultaJuros = new TipoNFeDebito("04", "Multa Juros");
    public static final TipoNFeDebito tdTransferenciaCreditoSucessao = new TipoNFeDebito("05", "Transferência Crédito Sucessão");

    private final String value;
    private final String description;

    static {
        addToLookup(tdNenhum);
        addToLookup(tdTransferenciaCreditoCooperativa);
        addToLookup(tdAnulacao);
        addToLookup(tdDebitosNaoProcessadas);
        addToLookup(tdMultaJuros);
        addToLookup(tdTransferenciaCreditoSucessao);
    }

    private TipoNFeDebito(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoNFeDebito tipoNFeDebito) {
        lookup.put(tipoNFeDebito.value, tipoNFeDebito);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoNFeDebito fromValue(String value) {
        TipoNFeDebito tipoNFeDebito = lookup.get(value);
        if (tipoNFeDebito == null) {
            throw new IllegalArgumentException("TipoNFeDebito inválido: " + value);
        }
        return tipoNFeDebito;
    }

    @Override
    public String toString() {
        return value;
    }

}
