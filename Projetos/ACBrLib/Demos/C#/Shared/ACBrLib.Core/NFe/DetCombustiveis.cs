using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetCombustiveis
    {
        dc_cProdANP,
        dc_CODIF,
        dc_qTemp,
        dc_UFCons,
        dc_CIDE,
        dc_qBCProd,
        dc_vAliqProd,
        dc_vCIDE
    }
}