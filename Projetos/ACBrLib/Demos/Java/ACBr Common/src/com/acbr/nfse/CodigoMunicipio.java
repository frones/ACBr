package com.acbr.nfse;

import com.sun.org.glassfish.gmbal.Description;
import java.util.HashMap;
import java.util.Map;

public enum CodigoMunicipio {
     
    NenhumaCidadeSelecionada(0),
    
    //Rondônia
    Ariquemes_RO(1100023),
    Cabixi_RO(1100031),
    Cacoal_RO(1100049),
    Colorado_do_Oeste_RO(1100064),
    Guajara_Mirim_RO(1100106),
    Jaru_RO(1100114),
    JiParana_RO(1100122),
    Machadinho_do_Oeste_RO(1100130),
    Ouro_Preto_do_Oeste_RO(1100155),
    Pimenta_Buendo_RO(1100189),
    Porto_Velho_RO(1100205),
    Presidente_Medici_RO(1100254),
    Vilhena_RO(1100304),
    Alto_Paraiso_RO(1100403),
    Buritis_RO(1100452),
    Cujubim_RO(1100940),
    Itapua_do_Oeste_RO(1101104),
    ValedoAnari_RO(1101757),
    
    //Acre
    Rio_Branco_AC(1200401),
    Rodrigues_Alves_AC(1200427),
    Senador_Guiomard_AC(1200450),
    Tarauaca_AC(1200609),
    
    //Amazonas
    Iranduba_AM(1301852),
    Manaus_AM(1302603),
    
    //Roraima
    Boa_Vista_RR(1400100),
    Bonfim_RR(1400159),
    Caracarai_RR(1400209),
    
    //Distrito Federal
    Brasilia_DF(5300108);
    
    
    
    
    
    public static final Map<Integer, CodigoMunicipio> map;
    public final int enumValue;
    
    static {
        map = new HashMap<>();
        for (CodigoMunicipio value : CodigoMunicipio.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static CodigoMunicipio valueOf(int value){
        return map.get(value);
    }
    
    CodigoMunicipio(int id){
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
