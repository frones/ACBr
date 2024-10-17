package com.acbr.nfe.acbrlibnfe.demo.utils;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ACBrLibHelper {

    private static ACBrLibNFe ACBrNFe;

    public static ACBrLibNFe getInstance(String fileConfig) {
        if (ACBrNFe == null) {
            ACBrNFe = new ACBrLibNFe(fileConfig, "");
        }
        return ACBrNFe;
    }
}
