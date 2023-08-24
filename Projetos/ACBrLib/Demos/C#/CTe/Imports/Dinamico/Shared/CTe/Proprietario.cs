using ACBrLib.Core.CTe;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.CTe
{
    public class Proprietario
    {       
        public string CNPJCPF { get; set; }

        public string TAF { get; set; }

        public string NroRegEstadual { get; set; }

        public string xNome { get; set; }

        public string IE { get; set; }

        public string UF { get; set; }

        public TipoProprietario tpProp { get; set; }
    }
}
