using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.CTe;

namespace ACBrLib.Core.CTe
{
    public class InfUnidTranspCTe
    {
        public TipoUnidTranspCTe tpUnidTransp { get; set; }

        public string idUnidTransp { get; set; }

        public List<LacreUnidadeTranspCTe> lacUnidTransp { get; } = new List<LacreUnidadeTranspCTe>();

        public List<InfUnidCargaCTe> InfUnidCargaCTe { get; } = new List<InfUnidCargaCTe>();

        public decimal qtdRat { get; set; }
    }
}
