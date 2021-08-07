using ACBrLib.Core;

namespace ACBrLib.MDFe
{
    public sealed class InfMDFe
    {
        [IniKey("versao")]
        public VersaoMDFe Versao { get; set; } = VersaoMDFe.ve300;
    }
}