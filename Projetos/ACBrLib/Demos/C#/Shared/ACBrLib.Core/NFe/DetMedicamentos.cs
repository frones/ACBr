using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetMedicamentos
    {
        dm_nLote = 0,
        dm_qLote = 1 << 0,
        dm_dFab = 1 << 1,
        dm_dVal = 1 << 2,
        dm_vPMC = 1 << 3
    }
}