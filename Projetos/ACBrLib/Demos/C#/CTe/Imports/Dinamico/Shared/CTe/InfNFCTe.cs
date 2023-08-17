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

        public List<InfUnidCargaCTe> infUnidCarga { get; set; } = new List<InfUnidCargaCTe>();

        public List<InfUnidTranspCTe> infUnidTransp { get; set; } = new List<InfUnidTranspCTe>();
    }
}
