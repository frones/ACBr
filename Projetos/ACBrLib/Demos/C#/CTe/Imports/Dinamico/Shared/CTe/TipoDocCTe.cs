using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public enum TipoDocCTe
    {
        [EnumValue("00")]
        declaracao = 00,

        [EnumValue("10")]
        dutoviario = 10,

        [EnumValue("59")]
        CFeSAT = 59,

        [EnumValue("65")]
        NFCe = 65,

        [EnumValue("99")]
        outros = 99
    }
}
