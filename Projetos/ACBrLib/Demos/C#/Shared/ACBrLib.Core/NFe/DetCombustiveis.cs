using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetCombustiveis
    {
        dc_cProdANP = 0,
        dc_CODIF = 1 << 0,
        dc_qTemp = 1 << 1,
        dc_UFCons = 1 << 2,
        dc_CIDE = 1 << 3,
        dc_qBCProd = 1 << 4,
        dc_vAliqProd = 1 << 5,
        dc_vCIDE = 1 << 6
    }
}