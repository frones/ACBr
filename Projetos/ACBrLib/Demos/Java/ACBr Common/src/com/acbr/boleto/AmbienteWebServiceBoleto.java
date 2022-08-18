package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum AmbienteWebServiceBoleto {
    taProducao(0),
    taHomologacao(1);
    
    private static final Map<Integer, AmbienteWebServiceBoleto> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (AmbienteWebServiceBoleto value : AmbienteWebServiceBoleto.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static AmbienteWebServiceBoleto valueOf(int value) {
        return map.get(value);
    }
    
    private AmbienteWebServiceBoleto (int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
