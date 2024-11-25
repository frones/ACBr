using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetMedicamentos
    {
        dm_nLote = 1 << 0,
        dm_qLote = 1 << 1,
        dm_dFab = 1 << 2,
        dm_dVal = 1 << 3,
        dm_vPMC = 1 << 4
    }
}