using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public sealed class TomadorCTe
    {
        public enum toma
        {
            remetende = 0,
            expedidor = 1,
            recebedor = 2,
            destinatario = 3,
            outros = 4,
        }

        public long CNPJCPF { get; set; }

        public string IE { get; set; }

        public string xNome { get; set; }

        public string xFant { get; set; }

        public int fone { get; set; }

        public EnderecoTomadorCTe enderToma { get; set; }
     
        public string email { get; set; }
    }
}
