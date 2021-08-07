using ACBrLib.Core;

namespace ACBrLib.MDFe
{
    public enum TpCarga
    {
        [EnumValue("01")]
        tcGranelSolido,

        [EnumValue("02")]
        tcGranelLiquido,

        [EnumValue("03")]
        tcFrigorificada,

        [EnumValue("04")]
        tcConteinerizada,

        [EnumValue("05")]
        tcCargaGeral,

        [EnumValue("06")]
        tcNeogranel,

        [EnumValue("07")]
        tcPerigosaGranelSolido,

        [EnumValue("08")]
        tcPerigosaGranelLiquido,

        [EnumValue("09")]
        tcPerigosaCargaFrigorificada,

        [EnumValue("10")]
        tcPerigosaConteinerizada,

        [EnumValue("11")]
        tcPerigosaCargaGeral
    }
}