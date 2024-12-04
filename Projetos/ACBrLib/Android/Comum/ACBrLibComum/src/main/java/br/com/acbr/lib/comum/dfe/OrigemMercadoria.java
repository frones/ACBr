package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class OrigemMercadoria {

    private static final Map<String, OrigemMercadoria> lookup = new HashMap<>();

    public static final OrigemMercadoria oeNacional = new OrigemMercadoria("0", "Nacional");
    public static final OrigemMercadoria oeEstrangeiraImportacaoDireta = new OrigemMercadoria("1", "Estrangeira - Importação direta");
    public static final OrigemMercadoria oeEstrangeiraAdquiridaBrasil = new OrigemMercadoria("2", "Estrangeira - Adquirida no Brasil");
    public static final OrigemMercadoria oeNacionalConteudoImportacaoSuperior40 = new OrigemMercadoria("3", "Nacional - Conteúdo de importação superior a 40%");
    public static final OrigemMercadoria oeNacionalProcessosBasicos = new OrigemMercadoria("4", "Nacional - Processos básicos");
    public static final OrigemMercadoria oeNacionalConteudoImportacaoInferiorIgual40 = new OrigemMercadoria("5", "Nacional - Conteúdo de importação inferior ou igual a 40%");
    public static final OrigemMercadoria oeEstrangeiraImportacaoDiretaSemSimilar = new OrigemMercadoria("6", "Estrangeira - Importação direta sem similar");
    public static final OrigemMercadoria oeEstrangeiraAdquiridaBrasilSemSimilar = new OrigemMercadoria("7", "Estrangeira - Adquirida no Brasil sem similar");
    public static final OrigemMercadoria oeNacionalConteudoImportacaoSuperior70 = new OrigemMercadoria("8", "Nacional - Conteúdo de importação superior a 70%");

    private final String value;
    private final String description;

    static{
        addToLookup(oeNacional);
        addToLookup(oeEstrangeiraImportacaoDireta);
        addToLookup(oeEstrangeiraAdquiridaBrasil);
        addToLookup(oeNacionalConteudoImportacaoSuperior40);
        addToLookup(oeNacionalProcessosBasicos);
        addToLookup(oeNacionalConteudoImportacaoInferiorIgual40);
        addToLookup(oeEstrangeiraImportacaoDiretaSemSimilar);
        addToLookup(oeEstrangeiraAdquiridaBrasilSemSimilar);
        addToLookup(oeNacionalConteudoImportacaoSuperior70);
    }

    private OrigemMercadoria(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(OrigemMercadoria origemMercadoria) {
        lookup.put(origemMercadoria.value, origemMercadoria);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static OrigemMercadoria fromValue(String value) {
        OrigemMercadoria origemMercadoria = lookup.get(value);
        if (origemMercadoria == null) {
            throw new IllegalArgumentException("Origem Mercadoria inválida: " + value);
        }
        return origemMercadoria;
    }

    @Override
    public String toString() {
        return value;
    }
}
