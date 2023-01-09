using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.CTe;

namespace ACBrLib.Core.CTe
{
    public class InfUnidCargaCTe
    {
        public TipoUnidCargaCTe tpUnidCargaCTe { get; set; }

        public string idUnidCarga { get; set; }

        public LacreUnidadeCargaCTe lacUnidCarga { get; set; }

        public decimal qtdRat { get; set; }
    }
}
