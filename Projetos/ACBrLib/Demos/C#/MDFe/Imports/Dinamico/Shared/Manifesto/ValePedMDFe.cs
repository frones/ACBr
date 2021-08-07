using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class ValePedMDFe
    {
        public CategCombVeic CategCombVeic { get; set; } = CategCombVeic.tcNenhum;

        public List<DispMDFe> disp { get; } = new List<DispMDFe>();
    }
}