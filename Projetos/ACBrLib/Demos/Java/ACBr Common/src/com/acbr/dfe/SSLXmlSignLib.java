package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum SSLXmlSignLib {
    xsNone(0),
    xsXmlSec(1),
    xsMsXml(2),
    xsMsXmlCapicom(3),
    xsLibXml2(4);
    
    private static final Map<Integer, SSLXmlSignLib> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (SSLXmlSignLib value : SSLXmlSignLib.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static SSLXmlSignLib valueOf(int value) {
        return map.get(value);
    }

    private SSLXmlSignLib(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
