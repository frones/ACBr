package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class DeterminacaoBaseIcmsST {

    private static final Map<String, DeterminacaoBaseIcmsST> lookup = new HashMap<>();

    public static final DeterminacaoBaseIcmsST dbisPrecoTabelado = new DeterminacaoBaseIcmsST("0", "Preço Tabelado");
    public static final DeterminacaoBaseIcmsST dbisListaNegativa = new DeterminacaoBaseIcmsST("1", "Lista Negativa");
    public static final DeterminacaoBaseIcmsST dbisListaPositiva = new DeterminacaoBaseIcmsST("2", "Lista Positiva");
    public static final DeterminacaoBaseIcmsST dbisListaNeutra = new DeterminacaoBaseIcmsST("3", "Lista Neutra");
    public static final DeterminacaoBaseIcmsST dbisMargemValorAgregado = new DeterminacaoBaseIcmsST("4", "Margem de Valor Agregado");
    public static final DeterminacaoBaseIcmsST dbisPauta = new DeterminacaoBaseIcmsST("5", "Pauta");
    public static final DeterminacaoBaseIcmsST dbisValordaOperacao = new DeterminacaoBaseIcmsST("6", "Valor da Operação");

    private final String value;
    private final String description;

    static {
        addToLookup(dbisPrecoTabelado);
        addToLookup(dbisListaNegativa);
        addToLookup(dbisListaPositiva);
        addToLookup(dbisListaNeutra);
        addToLookup(dbisMargemValorAgregado);
        addToLookup(dbisPauta);
        addToLookup(dbisValordaOperacao);
    }

    private DeterminacaoBaseIcmsST(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(DeterminacaoBaseIcmsST dbis) {
        lookup.put(dbis.value, dbis);
    }

    public static DeterminacaoBaseIcmsST get(String value) {
        return lookup.get(value);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static DeterminacaoBaseIcmsST valueOf(String value) {
        DeterminacaoBaseIcmsST dbis = lookup.get(value);
        if (dbis == null) {
            throw new IllegalArgumentException("DeterminacaoBaseIcmsST inválido: " + value);
        }
        return dbis;
    }

    @Override
    public String toString() {
        return description;
    }
}
