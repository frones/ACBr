using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class InfDocCTe
    {
        public InfNFeCTe infNFe { get; set; } = new InfNFeCTe();

        public InfOutrosCTe infOutros { get; set; } = new InfOutrosCTe();
    }
}
