package com.acbr.pixcd.acbrlibpixcd.demo.utils;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ACBrLibHelper {

    private static ACBrLibPIXCD ACBrPIXCD;

    public static ACBrLibPIXCD getInstance(String fileConfig){
        if (ACBrPIXCD == null) {
            ACBrPIXCD = new ACBrLibPIXCD(fileConfig, "");
        }
        return ACBrPIXCD;
    }
}
