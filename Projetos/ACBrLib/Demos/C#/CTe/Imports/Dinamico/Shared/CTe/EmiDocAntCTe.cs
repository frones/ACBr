using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class EmiDocAntCTe
    {
        public long CNPJCPF { get; set; }

        public int IE { get; set; }

        public string UF { get; set; }

        public string xNome { get; set; }

        public IdDocAntPapCTe idDocAntPap { get; set; }
        
        public IdDocAntEleCTe idDocAntEle { get; set; }
    }
}
