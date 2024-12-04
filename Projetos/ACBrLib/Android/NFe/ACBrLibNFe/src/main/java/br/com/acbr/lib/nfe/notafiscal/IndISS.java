package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class IndISS {

    private static final Map<String, IndISS> lookup = new HashMap<>();

    public static final IndISS iiExigivel = new IndISS("1", "Exigível");
    public static final IndISS iiNaoIncidencia = new IndISS("2", "Não incidência");
    public static final IndISS iiIsencao = new IndISS("3", "Isenção");
    public static final IndISS iiExportacao = new IndISS("4", "Exportação");
    public static final IndISS iiImunidade = new IndISS("5", "Imune à incidência");
    public static final IndISS iiExigSuspDecisaoJudicial = new IndISS("6", "Exigibilidade de Suspensão por Decisão Judicial");
    public static final IndISS iiExigSuspProcessoAdm = new IndISS("7", "Exigibilidade de Suspensão por Processo Administrativo");

    private final String value;
    private final String description;

    static {
        addToLookup(iiExigivel);
        addToLookup(iiNaoIncidencia);
        addToLookup(iiIsencao);
        addToLookup(iiExportacao);
        addToLookup(iiImunidade);
        addToLookup(iiExigSuspDecisaoJudicial);
        addToLookup(iiExigSuspProcessoAdm);
    }

    private IndISS(String value, String description){
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(IndISS indISS){
        lookup.put(indISS.value, indISS);
    }

    public String getValue(){
        return value;
    }

    public String getDescription(){
        return description;
    }

    public static IndISS fromValue(String value){
        IndISS indISS = lookup.get(value);
        if (indISS == null){
            throw new IllegalArgumentException("IndISS inválido: " + value);
        }
        return indISS;
    }

    @Override
   public String toString(){
        return value;
    }
}
