using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class DestinatarioCTe
    {
        public long CNPJCPF { get; set; }

        public string IE { get; set; }

        public string xNome { get; set; }

        public int fone { get; set; }

        public int ISUF { get; set; }

        public EnderDestCTe enderDest { get; set; }

        public string email { get; set; }
    }
}
