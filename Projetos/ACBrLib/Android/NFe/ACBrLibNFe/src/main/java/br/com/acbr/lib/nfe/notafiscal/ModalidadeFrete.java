package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class ModalidadeFrete {

    private static final Map<String, ModalidadeFrete> lookup = new HashMap<>();

    public static final ModalidadeFrete mfContaEmitente = new ModalidadeFrete("0", "Conta do Emitente");
    public static final ModalidadeFrete mfContaDestinatario = new ModalidadeFrete("1", "Conta do Destinatário");
    public static final ModalidadeFrete mfContaTerceiros = new ModalidadeFrete("2", "Conta dos Terceiros");
    public static final ModalidadeFrete mfProprioRemetente = new ModalidadeFrete("3", "Proprio Remetente");
    public static final ModalidadeFrete mfProprioDestinatario = new ModalidadeFrete("4", "Proprio Destinatário");
    public static final ModalidadeFrete mfSemFrete = new ModalidadeFrete("9", "Sem Frete");

    private final String value;
    private final String description;

    static {
        addToLookup(mfContaEmitente);
        addToLookup(mfContaDestinatario);
        addToLookup(mfContaTerceiros);
        addToLookup(mfProprioRemetente);
        addToLookup(mfProprioDestinatario);
        addToLookup(mfSemFrete);
    }

    private ModalidadeFrete(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(ModalidadeFrete modalidade) {
        lookup.put(modalidade.value, modalidade);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static ModalidadeFrete fromValue(String value) {
        ModalidadeFrete modalidade = lookup.get(value);
        if (modalidade == null) {
            throw new IllegalArgumentException("Modalidade de Frete inválida: " + value);
        }
        return modalidade;
    }

    @Override
    public String toString() {
        return value;
    }
}
