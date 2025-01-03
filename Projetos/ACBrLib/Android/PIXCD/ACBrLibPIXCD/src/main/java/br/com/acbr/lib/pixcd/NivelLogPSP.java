package br.com.acbr.lib.pixcd;

import java.util.HashMap;
import java.util.Map;

public class NivelLogPSP {

    private static final Map<String, NivelLogPSP> lookup = new HashMap<>();
    private static final NivelLogPSP[] allValues;

    public static final NivelLogPSP logPSPNenhum = new NivelLogPSP("0", "Nenhum");
    public static final NivelLogPSP logPSPBaixo = new NivelLogPSP("1", "Baixo");
    public static final NivelLogPSP logPSPNormal = new NivelLogPSP("2", "Normal");
    public static final NivelLogPSP logPSPAlto = new NivelLogPSP("3", "Alto");
    public static final NivelLogPSP logPSPMuitoAlto = new NivelLogPSP("4", "Muito Alto");

    private final String value;
    private final String description;

    static{
        addToLookup(logPSPNenhum);
        addToLookup(logPSPBaixo);
        addToLookup(logPSPNormal);
        addToLookup(logPSPAlto);
        addToLookup(logPSPMuitoAlto);
        allValues = new NivelLogPSP[]{logPSPNenhum, logPSPBaixo, logPSPNormal, logPSPAlto, logPSPMuitoAlto};
    }

    private NivelLogPSP(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(NivelLogPSP nivelLogPSP) {
        lookup.put(nivelLogPSP.value, nivelLogPSP);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static NivelLogPSP fromValue(String value){
        NivelLogPSP nivelLogPSP = lookup.get(value);
        if (nivelLogPSP == null){
            throw new IllegalArgumentException("Nivel Log do PSP inválido: " + value);
        }
        return nivelLogPSP;
    }

    public static NivelLogPSP[] values(){
        return allValues.clone();
    }

    @Override
    public String toString(){
        return value;
    }
}
