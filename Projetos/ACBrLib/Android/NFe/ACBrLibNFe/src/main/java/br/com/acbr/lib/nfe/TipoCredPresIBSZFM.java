package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class TipoCredPresIBSZFM {

    private static final Map<String, TipoCredPresIBSZFM> lookup = new HashMap<>();

    public static final TipoCredPresIBSZFM tcpNenhum = new TipoCredPresIBSZFM("", "Nenhum");
    public static final TipoCredPresIBSZFM tcpSemCredito = new TipoCredPresIBSZFM("0", "Sem Crédito");
    public static final TipoCredPresIBSZFM tcpBensConsumoFinal = new TipoCredPresIBSZFM("1", "Bens Consumo Final");
    public static final TipoCredPresIBSZFM tcpBensCapital = new TipoCredPresIBSZFM("2", "Bens Capital");
    public static final TipoCredPresIBSZFM tcpBensIntermediarios = new TipoCredPresIBSZFM("3", "Bens Intermediarios");
    public static final TipoCredPresIBSZFM tcpBensInformaticaOutros = new TipoCredPresIBSZFM("4", "Bens Informática e Outros");

    private final String value;
    private final String description;

    static {
        addToLookup(tcpNenhum);
        addToLookup(tcpSemCredito);
        addToLookup(tcpBensConsumoFinal);
        addToLookup(tcpBensCapital);
        addToLookup(tcpBensIntermediarios);
        addToLookup(tcpBensInformaticaOutros);
    }

    private TipoCredPresIBSZFM(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoCredPresIBSZFM tipoCredPresIBSZFM) {
        lookup.put(tipoCredPresIBSZFM.value, tipoCredPresIBSZFM);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoCredPresIBSZFM fromValue(String value) {
        TipoCredPresIBSZFM tipoCredPresIBSZFM = lookup.get(value);
        if (tipoCredPresIBSZFM == null) {
            throw new IllegalArgumentException("TipoCredPresIBSZFM inválido: " + value);
        }
        return tipoCredPresIBSZFM;
    }

    @Override
    public String toString() {
        return value;
    }
}
