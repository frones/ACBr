using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class InfNFeCTe
    {
        public string PIN { get; set; }

        public string chave { get; set; }

        public DateTime dPrev { get; set; }

        public InfUnidCargaCTe infUnidCarga { get; set; } = new InfUnidCargaCTe();

        public InfUnidTranspCTe infUnidTransp { get; set; } = new InfUnidTranspCTe();
    }
}
