using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class InformacoesRelativasImpostosCTe
    {
        public ICMSCTe ICMS00 { get; set; }

        public ICMSCTe ICMS20 { get; set; }

        public ICMSCTe ICMS45 { get; set; }

        public ICMSCTe ICMS60 { get; set; }

        public ICMSCTe ICMS90 { get; set; }

        public ICMSCTe ICMSOutraUF { get; set; }

        public ICMSCTe ICMSSN { get; set; }

        public decimal vTotTrib { get; set; }

        public string infAdFisco { get; set; }

        public ICMSUFFimCTe ICMSUFFim { get; set; }
    }
}
