package br.com.acbr.lib.nfe.notafiscal;

public enum TipoRef {
    NF("NF"),
    NFe("NFe"),
    SAT("SAT"),
    NFP("NFP"),
    CTe("CTe"),
    ECF("ECF");

    private final String value;

    TipoRef(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static TipoRef fromValue(String value) {
        for (TipoRef tipo : TipoRef.values()) {
            if (tipo.value.equals(value)) {
                return tipo;
            }
        }
        throw new IllegalArgumentException("Valor inválido: " + value);
    }
}
