package br.com.acbr.lib.nfe;

import java.util.HashMap;
import java.util.Map;

public class TipoEmissao {

    private static final Map<String, TipoEmissao> lookup = new HashMap<>();
    private static final TipoEmissao[] allValues;

    public static final TipoEmissao teNormal = new TipoEmissao("1", "Emissão normal (não em contingência)");
    public static final TipoEmissao teContingencia = new TipoEmissao("2", "Contingência FS-IA, com impressão do DANFE em formulário de segurança");
    public static final TipoEmissao teSCAN = new TipoEmissao("3", "Contingência SCAN(Sistema de Contingência do Ambiente Nacional)");
    public static final TipoEmissao teDPEC = new TipoEmissao("4", "Contingência DPEC(Declaração Prévia da Emissão em Contingência)");
    public static final TipoEmissao teFSDA = new TipoEmissao("5", "Contingência FS-DA, com impressão do DANFE em formulário de segurança");
    public static final TipoEmissao teSVCAN = new TipoEmissao("6", "Contingência SVC-AN(SEFAZ Virtual de Contingência do AN)");
    public static final TipoEmissao teSVCRS = new TipoEmissao("7", "Contingência SVC-RS(SEFAZ Virtual de Contingência do RS)");
    public static final TipoEmissao teSVCSP = new TipoEmissao("8", "Contingência SVC-SP(SEFAZ Virtual de Contingência de SP)");
    public static final TipoEmissao teOffLine = new TipoEmissao("9", "Contingência off-line da NFC-e(as demais opções de contingência são válidas também para a NFC-e)");

    private final String value;
    private final String description;

    static {
        addToLookup(teNormal);
        addToLookup(teContingencia);
        addToLookup(teSCAN);
        addToLookup(teDPEC);
        addToLookup(teFSDA);
        addToLookup(teSVCAN);
        addToLookup(teSVCRS);
        addToLookup(teSVCSP);
        addToLookup(teOffLine);
        allValues = new TipoEmissao[] { teNormal, teContingencia, teSCAN, teDPEC, teFSDA, teSVCAN, teSVCRS, teSVCSP, teOffLine };
    }

    private TipoEmissao(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoEmissao tipoEmissao) {
        lookup.put(tipoEmissao.value, tipoEmissao);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoEmissao fromValue(String value){
        TipoEmissao tipoEmissao = lookup.get(value);
        if (tipoEmissao == null){
            throw new IllegalArgumentException("Tipo de Emissão inválido: " + value);
        }
        return tipoEmissao;
    }

    public static TipoEmissao[] values(){
        return allValues.clone();
    }

    @Override
    public String toString(){
        return value;
    }
}
