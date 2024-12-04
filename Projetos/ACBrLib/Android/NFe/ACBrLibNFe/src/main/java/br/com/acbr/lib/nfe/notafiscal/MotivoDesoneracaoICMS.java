package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class MotivoDesoneracaoICMS {

    private static final Map<String, MotivoDesoneracaoICMS> lookup = new HashMap<>();

    public static final MotivoDesoneracaoICMS mdiTaxi = new MotivoDesoneracaoICMS("1", "Taxi");
    public static final MotivoDesoneracaoICMS mdiDeficienteFisico = new MotivoDesoneracaoICMS("2", "Deficiente Físico");
    public static final MotivoDesoneracaoICMS mdiProdutorAgropecuario = new MotivoDesoneracaoICMS("3", "Produtor Agropecuário");
    public static final MotivoDesoneracaoICMS mdiFrotistaLocadora = new MotivoDesoneracaoICMS("4", "Frotista/Locadora");
    public static final MotivoDesoneracaoICMS mdiDiplomaticoConsular = new MotivoDesoneracaoICMS("5", "Diplomático/Consular");
    public static final MotivoDesoneracaoICMS mdiAmazoniaLivreComercio = new MotivoDesoneracaoICMS("6", "Amazonia Livre/Comercio");
    public static final MotivoDesoneracaoICMS mdiSuframa = new MotivoDesoneracaoICMS("7", "Suframa");
    public static final MotivoDesoneracaoICMS mdiVendaOrgaosPublicos = new MotivoDesoneracaoICMS("8", "Venda de Orgaos Públicos");
    public static final MotivoDesoneracaoICMS mdiOutros = new MotivoDesoneracaoICMS("9", "Outros");
    public static final MotivoDesoneracaoICMS mdiDeficienteCondutor = new MotivoDesoneracaoICMS("10", "Deficiente Condutor");
    public static final MotivoDesoneracaoICMS mdiDeficienteNaoCondutor = new MotivoDesoneracaoICMS("11", "Deficiente Não Condutor");
    public static final MotivoDesoneracaoICMS mdiOrgaoFomento = new MotivoDesoneracaoICMS("12", "Órgão de Fomento");
    public static final MotivoDesoneracaoICMS mdiOlimpiadaRio2016 = new MotivoDesoneracaoICMS("16", "Olimpiada Rio 2016");
    public static final MotivoDesoneracaoICMS mdiSolicitadoFisco = new MotivoDesoneracaoICMS("90", "Solicitado pelo Fisco");

    private final String value;
    private final String description;

    static {
        addToLookup(mdiTaxi);
        addToLookup(mdiDeficienteFisico);
        addToLookup(mdiProdutorAgropecuario);
        addToLookup(mdiFrotistaLocadora);
        addToLookup(mdiDiplomaticoConsular);
        addToLookup(mdiAmazoniaLivreComercio);
        addToLookup(mdiSuframa);
        addToLookup(mdiVendaOrgaosPublicos);
        addToLookup(mdiOutros);
        addToLookup(mdiDeficienteCondutor);
        addToLookup(mdiDeficienteNaoCondutor);
        addToLookup(mdiOrgaoFomento);
        addToLookup(mdiOlimpiadaRio2016);
        addToLookup(mdiSolicitadoFisco);
    }

    private MotivoDesoneracaoICMS(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(MotivoDesoneracaoICMS motivo) {
        lookup.put(motivo.value, motivo);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static MotivoDesoneracaoICMS fromValue(String value) {
        MotivoDesoneracaoICMS motivo = lookup.get(value);
        if (motivo == null) {
            throw new IllegalArgumentException("Motivo de Desoneração de ICMS inválido: " + value);
        }
        return motivo;
    }

    @Override
    public String toString() {
        return value;
    }
}
