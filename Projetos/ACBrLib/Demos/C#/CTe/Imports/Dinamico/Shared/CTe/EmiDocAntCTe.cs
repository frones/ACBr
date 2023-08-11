using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class EmiDocAntCTe
    {
        public string CNPJCPF { get; set; }

        public string IE { get; set; }

        public string UF { get; set; }

        public string xNome { get; set; }

        public List<IdDocAntPapCTe> idDocAntPap { get; set; } = new List<IdDocAntPapCTe>();

        public List<IdDocAntEleCTe> idDocAntEle { get; set; } = new List<IdDocAntEleCTe>();     
    }
}
