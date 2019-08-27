package com.acbr.posprinter;

import java.util.HashMap;
import java.util.Map;

public enum SerialParity {
    None('N'),
    Odd('O'),
    Even('E'),
    Mark('M'),
    Space('S');

    private static final Map<Character, SerialParity> map;

    static {
        map = new HashMap<>();
        for (SerialParity parity : SerialParity.values()) {
            map.put(parity.asChar, parity);
        }
    }

    public static SerialParity valueOf(char parity) {
        return map.get(parity);
    }

    public char asChar() {
        return asChar;
    }

    private final char asChar;

    SerialParity(char asChar) {
        this.asChar = asChar;
    }
}
