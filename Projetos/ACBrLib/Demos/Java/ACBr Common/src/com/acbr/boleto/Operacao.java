package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum Operacao {
    tpInclui(0),
    tpAltera(1),
    tpBaixa(2),
    tpConsulta(3),
    tpConsultaDetalhe(4),
    tpPIXCriar(5),
    tpPIXCancelar(6),
    tpPIXConsultar(7);
    
    private static final Map<Integer, Operacao> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (Operacao value : Operacao.values() ){
            map.put(value.asInt(), value);
        }
    }
    
    public static Operacao valueOf(int value) {
        return map.get(value);
    }
    
    private Operacao (int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
    
}
