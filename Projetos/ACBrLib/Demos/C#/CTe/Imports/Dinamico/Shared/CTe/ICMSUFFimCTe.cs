using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ICMSUFFimCTe
    {
        public CSTCTe CST { get; set; }

        public decimal vBCUFFim { get; set; }
        
        public decimal pFCPUFFim { get; set; }

        public decimal pICMSUFFim { get; set; }

        public decimal pICMSInter { get; set; }

        public decimal vFCPUFFim { get; set; }

        public decimal vICMSUFFim { get; set; }

        public decimal vICMSUFIni { get; set; }
    }
}
