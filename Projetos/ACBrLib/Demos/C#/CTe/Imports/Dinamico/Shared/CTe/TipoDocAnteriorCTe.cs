using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public enum TipoDocAnteriorCTe
    {
        [EnumValue("07")]
        ATRE = 07,

        [EnumValue("08")]
        DTA = 08,

        [EnumValue("09")]
        ConhecimentoAereoInternacional = 09,

        [EnumValue("10")]
        ConhecimentoCartaPorteInternacional = 10,

        [EnumValue("11")]
        ConhecimentoAvulso = 11,

        [EnumValue("12")]
        TIF = 12,

        [EnumValue("13")]
        BL = 13
    }
}
