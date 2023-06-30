using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class InfDocCTe
    {
        public List<InfNFCTe> infNF { get; set; } = new List<InfNFCTe>();

        public List<InfNFeCTe> infNFe { get; set; } = new List<InfNFeCTe>();

        public InfOutrosCTe infOutros { get; set; } = new InfOutrosCTe();
    }
}
