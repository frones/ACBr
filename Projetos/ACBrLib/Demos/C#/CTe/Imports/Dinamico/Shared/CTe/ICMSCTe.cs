using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ICMSCTe
    {
        public decimal vBC { get; set; }

        public decimal pICMS { get; set; }

        public decimal vICMS { get; set; }

        public decimal pRedBC { get; set; }
        
        public decimal vBCSTRet { get; set; }

        public decimal vICMSSTRet { get; set; }

        public decimal pICMSSTRet { get; set; }

        public decimal vCred { get; set; }

        public decimal pRedBCOutraUF { get; set; }

        public decimal vBCOutraUF { get; set; }

        public decimal pICMSOutraUF { get; set; }

        public decimal vICMSOutraUF { get; set; }

        public int indSN { get; set; }
    }
}
