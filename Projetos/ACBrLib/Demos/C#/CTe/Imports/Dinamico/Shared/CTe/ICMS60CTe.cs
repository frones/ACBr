using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ICMS60CTe
    {
        public CSTCTe CST { get; set; }

        public decimal vBCSTRet { get; set; }

        public decimal vICMSSTRet { get; set; }

        public decimal pICMSSTRet { get; set; }

        public decimal vCred { get; set; }
    }
}
