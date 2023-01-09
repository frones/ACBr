using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class RemetenteCTe
    {
        public long CNPJCPF { get; set; }

        public string IE { get; set; }

        public string xNome { get; set; }

        public string xFant { get; set; }

        public int fone { get; set; }

        public EnderRemeCTe enderReme { get; set; }

        public string email { get; set; }
    }
}
