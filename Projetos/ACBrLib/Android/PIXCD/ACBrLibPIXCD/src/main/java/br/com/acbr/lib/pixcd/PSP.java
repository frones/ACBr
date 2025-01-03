package br.com.acbr.lib.pixcd;

import java.util.HashMap;
import java.util.Map;

public class PSP {

    private static final Map<String, PSP> lookup = new HashMap<>();
    private static final PSP[] allValues;

    public static final PSP pspBradesco = new PSP("0", "Bradesco");
    public static final PSP pspItau = new PSP("1", "Itau");
    public static final PSP pspBancoBrasil = new PSP("2", "Banco do Brasil");
    public static final PSP pspSantander = new PSP("3", "Santander");
    public static final PSP pspShipay = new PSP("4", "Shipay");
    public static final PSP pspSicredi = new PSP("5", "Sicredi");
    public static final PSP pspSicoob = new PSP("6", "Sicoob");
    public static final PSP pspPagSeguro = new PSP("7", "PagSeguro");
    public static final PSP pspGerenciaNet = new PSP("8", "GerenciaNet");
    public static final PSP pspPixPDV = new PSP("9", "PixPDV");
    public static final PSP pspInter = new PSP("10", "Inter");
    public static final PSP pspAilos = new PSP("11", "Ailos");
    public static final PSP pspMatera = new PSP("12", "Matera");
    public static final PSP pspCielo = new PSP("13", "Cielo");
    public static final PSP pspMercadoPago = new PSP("14", "Mercado Pago");
    public static final PSP pspGate2All = new PSP("15", "Gate2All");
    public static final PSP pspBanrisul = new PSP("16", "Banrisul");
    public static final PSP pspC6Bank = new PSP("17", "C6Bank");

    private final String value;
    private final String description;

    static {
        addToLookup(pspBradesco);
        addToLookup(pspItau);
        addToLookup(pspBancoBrasil);
        addToLookup(pspSantander);
        addToLookup(pspShipay);
        addToLookup(pspSicredi);
        addToLookup(pspSicoob);
        addToLookup(pspPagSeguro);
        addToLookup(pspGerenciaNet);
        addToLookup(pspPixPDV);
        addToLookup(pspInter);
        addToLookup(pspAilos);
        addToLookup(pspMatera);
        addToLookup(pspCielo);
        addToLookup(pspMercadoPago);
        addToLookup(pspGate2All);
        addToLookup(pspBanrisul);
        addToLookup(pspC6Bank);
        allValues = new PSP[]{pspBradesco, pspItau, pspBancoBrasil, pspSantander, pspShipay, pspSicredi,
                pspSicoob, pspPagSeguro, pspGerenciaNet, pspPixPDV, pspInter, pspAilos, pspMatera, pspCielo,
                pspMercadoPago, pspGate2All, pspBanrisul, pspC6Bank};
    }

    private PSP (String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(PSP psp) {
        lookup.put(psp.value, psp);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static PSP fromValue(String value){
        PSP psp = lookup.get(value);
        if (psp == null){
            throw new IllegalArgumentException("PSP inválido: " + value);
        }
        return psp;
    }

    public static PSP[] values(){
        return allValues.clone();
    }

    @Override
    public String toString(){
        return value;
    }
}
