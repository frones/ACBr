package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum ModeloBoleto {
    lPadrao(0),
    lCarne(1),
    llFatura(2),
    lPadraoEntrega(3),
    lReciboTopo(4),
    lPadraoEntrega2(5),
    lFaturaDetal(6);    
    
    private static final Map<Integer, ModeloBoleto> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (ModeloBoleto value : ModeloBoleto.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static ModeloBoleto valueOf(int value) {
        return map.get(value);
    }

    private ModeloBoleto(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
