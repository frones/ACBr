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

        public LacreUnidadeTranspCTe lacUnidTransp { get; set; }

        public InfUnidCargaCTe InfUnidCargaCTe { get; set; }

        public decimal qtdRat { get; set; }
    }
}
