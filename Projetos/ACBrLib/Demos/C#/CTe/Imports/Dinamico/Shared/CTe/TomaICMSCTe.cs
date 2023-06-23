using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.CTe;

namespace ACBrLib.Core.CTe
{
    public class TomaICMSCTe
    {   
        public long refNFe { get; set; }

        public RefNFCTe refNF { get; set; } = new RefNFCTe();

        public long refCTe { get; set; }
    }
}
