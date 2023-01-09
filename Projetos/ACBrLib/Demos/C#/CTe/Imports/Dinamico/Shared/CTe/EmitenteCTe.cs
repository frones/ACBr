using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class EmitenteCTe
    {
        public long CNPJ { get; set; }

        public int IE { get; set; }

        public int IEST { get; set; }

        public string xNome { get; set; }

        public string xFant { get; set; }

        public EnderEmitCTe enderEmit { get; set; }
    }
}
