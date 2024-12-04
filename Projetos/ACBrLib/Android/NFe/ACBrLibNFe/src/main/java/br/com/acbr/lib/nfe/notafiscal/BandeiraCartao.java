package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class BandeiraCartao {

    private static final Map<String, BandeiraCartao> lookup = new HashMap<>();

    public static final BandeiraCartao bcVisa = new BandeiraCartao("01", "Visa");
    public static final BandeiraCartao bcMasterCard = new BandeiraCartao("02", "MasterCard");
    public static final BandeiraCartao bcAmericanExpress = new BandeiraCartao("03", "American Express");
    public static final BandeiraCartao bcSorocred = new BandeiraCartao("04", "Sorocred");
    public static final BandeiraCartao bcDinersClub = new BandeiraCartao("05", "Diners Club");
    public static final BandeiraCartao bcElo = new BandeiraCartao("06", "Elo");
    public static final BandeiraCartao bcHipercard = new BandeiraCartao("07", "Hipercard");
    public static final BandeiraCartao bcAura = new BandeiraCartao("08", "Aura");
    public static final BandeiraCartao bcCabal = new BandeiraCartao("09", "Cabal");
    public static final BandeiraCartao bcAlelo = new BandeiraCartao("10", "Alelo");
    public static final BandeiraCartao bcBanesCard = new BandeiraCartao("11", "Banes Card");
    public static final BandeiraCartao bcCalCard = new BandeiraCartao("12", "Cal Card");
    public static final BandeiraCartao bcCredz = new BandeiraCartao("13", "Credz");
    public static final BandeiraCartao bcDiscover = new BandeiraCartao("14", "Discover");
    public static final BandeiraCartao bcGoodCard = new BandeiraCartao("15", "Good Card");
    public static final BandeiraCartao bcGrenCard = new BandeiraCartao("16", "Gren Card");
    public static final BandeiraCartao bcHiper = new BandeiraCartao("17", "Hiper");
    public static final BandeiraCartao bcJcB = new BandeiraCartao("18", "JC B");
    public static final BandeiraCartao bcMais = new BandeiraCartao("19", "Mais");
    public static final BandeiraCartao bcMaxVan = new BandeiraCartao("20", "Max Van");
    public static final BandeiraCartao bcPolicard = new BandeiraCartao("21", "Policard");
    public static final BandeiraCartao bcRedeCompras = new BandeiraCartao("22", "Rede Compras");
    public static final BandeiraCartao bcSodexo = new BandeiraCartao("23", "Sodexo");
    public static final BandeiraCartao bcValeCard = new BandeiraCartao("24", "Vale Card");
    public static final BandeiraCartao bcVerocheque = new BandeiraCartao("25", "Verocheque");
    public static final BandeiraCartao bcVR = new BandeiraCartao("26", "VR");
    public static final BandeiraCartao bcTicket = new BandeiraCartao("27", "Ticket");
    public static final BandeiraCartao bcOutros = new BandeiraCartao("99", "Outros");

    private final String value;
    private final String description;

    static{
        addToLookup(bcVisa);
        addToLookup(bcMasterCard);
        addToLookup(bcAmericanExpress);
        addToLookup(bcSorocred);
        addToLookup(bcDinersClub);
        addToLookup(bcElo);
        addToLookup(bcHipercard);
        addToLookup(bcAura);
        addToLookup(bcCabal);
        addToLookup(bcAlelo);
        addToLookup(bcBanesCard);
        addToLookup(bcCalCard);
        addToLookup(bcCredz);
        addToLookup(bcDiscover);
        addToLookup(bcGoodCard);
        addToLookup(bcGrenCard);
        addToLookup(bcHiper);
        addToLookup(bcJcB);
        addToLookup(bcMais);
        addToLookup(bcMaxVan);
        addToLookup(bcPolicard);
        addToLookup(bcRedeCompras);
        addToLookup(bcSodexo);
        addToLookup(bcValeCard);
        addToLookup(bcVerocheque);
        addToLookup(bcVR);
        addToLookup(bcTicket);
        addToLookup(bcOutros);
    }

    private BandeiraCartao(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(BandeiraCartao bandeira) {
        lookup.put(bandeira.value, bandeira);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static BandeiraCartao fromValue(String value) {
        BandeiraCartao bandeira = lookup.get(value);
        if (bandeira == null) {
            throw new IllegalArgumentException("Bandeira de Cartão inválida: " + value);
        }
        return bandeira;
    }

    @Override
    public String toString() {
        return value;
    }
}
