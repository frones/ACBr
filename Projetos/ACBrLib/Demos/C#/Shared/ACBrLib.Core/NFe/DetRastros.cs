using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetRastros
    {
        dr_nLote = 1 << 0,
        dr_qLote = 1 << 1,
        dr_dFab = 1 << 2,
        dr_dVal = 1 << 3,
        dr_cAgreg = 1 << 4
    }
}