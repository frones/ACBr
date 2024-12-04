package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class ProcessoEmissao {

    private static final Map<String, ProcessoEmissao> lookup = new HashMap<>();

    public static final ProcessoEmissao peAplicativoContribuinte = new ProcessoEmissao("0", "Aplicação Fisco");
    public static final ProcessoEmissao peAvulsaFisco = new ProcessoEmissao("1", "Avulsa Fisco");
    public static final ProcessoEmissao peAvulsaContribuinte = new ProcessoEmissao("2", "Avulsa Contribuinte");
    public static final ProcessoEmissao peContribuinteAplicativoFisco = new ProcessoEmissao("3", "Contribuinte Aplicativo Fisco");

    private final String value;
    private final String description;

    static {
        addToLookup(peAplicativoContribuinte);
        addToLookup(peAvulsaFisco);
        addToLookup(peAvulsaContribuinte);
        addToLookup(peContribuinteAplicativoFisco);
    }

    private ProcessoEmissao(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(ProcessoEmissao processo) {
        lookup.put(processo.value, processo);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static ProcessoEmissao fromValue(String value) {
        ProcessoEmissao processo = lookup.get(value);
        if (processo == null) {
            throw new IllegalArgumentException("Processo de Emissão inválido: " + value);
        }
        return processo;
    }

    @Override
    public String toString() {
        return value;
    }
}
