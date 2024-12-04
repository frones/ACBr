package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class TipoViaTransp {

    private static final Map<String, TipoViaTransp> map = new HashMap<>();

    public static final TipoViaTransp tvMaritima = new TipoViaTransp("1", "Marítima");
    public static final TipoViaTransp tvFluvial = new TipoViaTransp("2", "Fluvial");
    public static final TipoViaTransp tvLacustre = new TipoViaTransp("3", "Lacustre");
    public static final TipoViaTransp tvAerea = new TipoViaTransp("4", "Aérea");
    public static final TipoViaTransp tvPostal = new TipoViaTransp("5", "Postal");
    public static final TipoViaTransp tvFerroviaria = new TipoViaTransp("6", "Ferroviária");
    public static final TipoViaTransp tvRodoviaria = new TipoViaTransp("7", "Rodoviária");
    public static final TipoViaTransp tvConduto = new TipoViaTransp("8", "Conduto");
    public static final TipoViaTransp tvMeiosProprios = new TipoViaTransp("9", "Meios Próprios");
    public static final TipoViaTransp tvEntradaSaidaFicta = new TipoViaTransp("10", "Entrada/Saída Ficta");
    public static final TipoViaTransp tvCourier = new TipoViaTransp("11", "Courier");
    public static final TipoViaTransp tvHandcarry = new TipoViaTransp("12", "Handcarry");

    private final String value;
    private final String description;

    static {
        addToLookup(tvMaritima);
        addToLookup(tvFluvial);
        addToLookup(tvLacustre);
        addToLookup(tvAerea);
        addToLookup(tvPostal);
        addToLookup(tvFerroviaria);
        addToLookup(tvRodoviaria);
        addToLookup(tvConduto);
        addToLookup(tvMeiosProprios);
        addToLookup(tvEntradaSaidaFicta);
        addToLookup(tvCourier);
        addToLookup(tvHandcarry);
    }

    private TipoViaTransp(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(TipoViaTransp tipoViaTransp) {
        map.put(tipoViaTransp.value, tipoViaTransp);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static TipoViaTransp fromValue(String value) {
        TipoViaTransp tipoViaTransp = map.get(value);
        if (tipoViaTransp == null) {
            throw new IllegalArgumentException("Tipo de Via de Transporte inválido: " + value);
        }
        return tipoViaTransp;
    }

    @Override
    public String toString() {
        return value;
    }
}
