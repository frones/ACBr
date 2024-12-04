package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class TipoDANFE {

    private static final Map<String, TipoDANFE> lookup = new HashMap<>();
    private static final TipoDANFE[] allValues;

    public static final TipoDANFE tiSemGeracao = new TipoDANFE("0", "Sem Impressão");
    public static final TipoDANFE tiRetrato = new TipoDANFE("1", "Retrato");
    public static final TipoDANFE tiPaisagem = new TipoDANFE("2", "Paisagem");
    public static final TipoDANFE tiSimplificado = new TipoDANFE("3", "Simplificado");
    public static final TipoDANFE tiNFCe = new TipoDANFE("4", "NFCe");
    public static final TipoDANFE tiMsgEletronica = new TipoDANFE("5", "Mensagem Eletrônica");

    private final String value;
    private final String description;

    static {
        addToLookup(tiSemGeracao);
        addToLookup(tiRetrato);
        addToLookup(tiPaisagem);
        addToLookup(tiSimplificado);
        addToLookup(tiNFCe);
        addToLookup(tiMsgEletronica);
        allValues = new TipoDANFE[] { tiSemGeracao, tiRetrato, tiPaisagem, tiSimplificado, tiNFCe, tiMsgEletronica };
    }

    private TipoDANFE(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoDANFE tipoDANFE){
        lookup.put(tipoDANFE.value, tipoDANFE);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoDANFE fromValue(String value) {
        TipoDANFE tipoDANFE = lookup.get(value);
        if (tipoDANFE == null) {
            throw new IllegalArgumentException("Tipo DANFE inválido: " + value);
        }
        return tipoDANFE;
    }

    public static TipoDANFE[] values() {
        return allValues.clone();
    }

    @Override
    public String toString() {
        return value;
    }
}
