package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class DeterminacaoBaseIcms {

    private static final Map<String, DeterminacaoBaseIcms> lookup = new HashMap<>();

    public static final DeterminacaoBaseIcms dbiNenhum = new DeterminacaoBaseIcms("", "Nenhum");
    public static final DeterminacaoBaseIcms dbiMargemValorAgregado = new DeterminacaoBaseIcms("0", "Margem de Valor Agregado");
    public static final DeterminacaoBaseIcms dbiPauta = new DeterminacaoBaseIcms("1", "Pauta");
    public static final DeterminacaoBaseIcms dbiPrecoTabelado = new DeterminacaoBaseIcms("2", "Preço Tabelado");
    public static final DeterminacaoBaseIcms dbiValorOperacao = new DeterminacaoBaseIcms("3", "Valor da Operação");

    private final String value;
    private final String description;

    static {
        addToLookup(dbiNenhum);
        addToLookup(dbiMargemValorAgregado);
        addToLookup(dbiPauta);
        addToLookup(dbiPrecoTabelado);
        addToLookup(dbiValorOperacao);
    }

    private DeterminacaoBaseIcms(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(DeterminacaoBaseIcms determinacao) {
        lookup.put(determinacao.value, determinacao);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static DeterminacaoBaseIcms fromValue(String value) {
        DeterminacaoBaseIcms determinacao = lookup.get(value);
        if (determinacao == null) {
            throw new IllegalArgumentException("Determinação de Base de ICMS inválida: " + value);
        }
        return determinacao;
    }

    @Override
    public String toString() {
        return value;
    }
}
