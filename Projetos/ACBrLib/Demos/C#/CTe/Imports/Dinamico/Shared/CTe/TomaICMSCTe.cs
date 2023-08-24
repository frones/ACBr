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
        public string refNFe { get; set; }

        public RefNFCTe refNF { get; set; } = new RefNFCTe();

        public string refCTe { get; set; }
    }
}
