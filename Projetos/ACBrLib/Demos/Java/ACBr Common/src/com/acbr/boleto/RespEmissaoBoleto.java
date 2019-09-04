package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum RespEmissaoBoleto {
    tbCliEmite(0),
    tbBancoEmite(1),
    tbBancoReemite(2),
    tbBancoNaoReemite(3);   
    
    private static final Map<Integer, RespEmissaoBoleto> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (RespEmissaoBoleto value : RespEmissaoBoleto.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static RespEmissaoBoleto valueOf(int value) {
        return map.get(value);
    }

    private RespEmissaoBoleto(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}