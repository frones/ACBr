using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ICMS90CTe
    {
        public CSTCTe CST { get; set; }

        public decimal pRedBC { get; set; }

        public decimal vBC { get; set; }

        public decimal pICMS { get; set; }

        public decimal vICMS { get; set; }

        public decimal vCred { get; set; }
    }
}
