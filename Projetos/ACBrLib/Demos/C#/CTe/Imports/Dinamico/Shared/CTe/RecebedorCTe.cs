using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class RecebedorCTe
    {
        public int CNPJCPF { get; set; }

        public string IE { get; set; }

        public string xNome { get; set; }

        public int fone { get; set; }

        public List<EnderRecebCTe> enderReceb { get; } = new List<EnderRecebCTe>();

        public string email { get; set; }
    }
}
