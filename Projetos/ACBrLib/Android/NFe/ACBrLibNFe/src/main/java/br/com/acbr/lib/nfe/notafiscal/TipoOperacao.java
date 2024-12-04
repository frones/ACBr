package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class TipoOperacao {

    private static final Map<String, TipoOperacao> lookup = new HashMap<>();

    public static final TipoOperacao toOutros = new TipoOperacao("0", "Outros");
    public static final TipoOperacao toVendaConcessionaria = new TipoOperacao("1", "Venda Concessionária");
    public static final TipoOperacao toFaturamentoDireto = new TipoOperacao("2", "Faturamento Direto");
    public static final TipoOperacao toVendaDireta = new TipoOperacao("3", "Venda Direta");

    private final String value;
    private final String description;

    static {
        addToLookup(toOutros);
        addToLookup(toVendaConcessionaria);
        addToLookup(toFaturamentoDireto);
        addToLookup(toVendaDireta);
    }

    private TipoOperacao(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoOperacao tipo) {
        lookup.put(tipo.value, tipo);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoOperacao fromValue(String value) {
        TipoOperacao tipo = lookup.get(value);
        if (tipo == null) {
            throw new IllegalArgumentException("Tipo de Operação inválido: " + value);
        }
        return tipo;
    }

    @Override
    public String toString() {
        return value;
    }
}
