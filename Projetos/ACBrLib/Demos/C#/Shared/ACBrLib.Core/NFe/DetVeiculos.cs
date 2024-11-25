using System;

namespace ACBrLib.Core.NFe
{
    [Flags]
    public enum DetVeiculos
    {
        dv_tpOp = 1 << 0,
        dv_chassi = 1 << 1,
        dv_cCor = 1 << 2,
        dv_xCor = 1 << 3,
        dv_pot = 1 << 4,
        dv_cilin = 1 << 5,
        dv_pesoL = 1 << 6,
        dv_pesoB = 1 << 7,
        dv_nSerie = 1 << 8,
        dv_tpComb = 1 << 9,
        dv_nMotor = 1 << 10,
        dv_CMT = 1 << 11,
        dv_dist = 1 << 12,
        dv_anoMod = 1 << 13,
        dv_anoFab = 1 << 14,
        dv_tpPint = 1 << 15,
        dv_tpVeic = 1 << 16,
        dv_espVeic = 1 << 17,
        dv_VIN = 1 << 18,
        dv_condVeic = 1 << 19,
        dv_cMod = 1 << 20,
        dv_cCorDENATRAN = 1 << 21,
        dv_lota = 1 << 22,
        dv_tpRest = 1 << 23
    }
}