using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class InfOutrosCTe
    {
        public TipoDocCTe tpDoc { get; set; }

        public string descOutros { get; set; }

        public string nDoc { get; set; }

        public DateTime dEmi { get; set; }

        public decimal vDocFisc { get; set; }

        public DateTime dPrev { get; set; }

        public List<InfUnidCargaCTe> InfUnidCarga { get; set; } = new List<InfUnidCargaCTe>();

        public List<InfUnidTranspCTe> InfUnidTransp { get; set; } = new List<InfUnidTranspCTe>();
    }
}
