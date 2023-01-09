using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ExpedidorCTe
    {
        public int CNPJCPF { get; set; }

        public string IE { get; set; }

        public string xNome { get; set; }

        public int fone { get; set; }

        public List<EnderExpedCTe> enderExped { get; } = new List<EnderExpedCTe>();

        public string email { get; set; }
    }
}
