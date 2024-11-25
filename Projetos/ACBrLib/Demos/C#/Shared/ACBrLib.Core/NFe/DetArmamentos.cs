using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetArmamentos
    {
        da_tpArma = 1 << 0,
        da_nSerie = 1 << 1,
        da_nCano = 1 << 2,
        da_descr = 1 << 3
    }
}