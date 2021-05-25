using System;
using System.ComponentModel;
using ACBrLib.Core.Ini;

namespace ACBrLib.Core.NFe
{
    public enum ModeloNFe
    {
        [EnumValue("55")]
        [Description("NFe - Nota Fiscal Eletrônica")]
        moNFe = 0,

        [EnumValue("65")]
        [Description("NFCe - Nota Fiscal do Consumidor Eletrônica")]
        moNFCe = 1
    }
}