using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class RodoviarioCTe
    {
        public string RNTRC { get; set; }

        public string NroRegEstadual { get; set; }

        public string TAF { get; set; }

        public VeiculosCTe veic { get; set; } = new VeiculosCTe();

        public InfFretamento infFretamento { get; set; } = new InfFretamento();

        public List<OrdensColetaCTe> occ { get; } = new List<OrdensColetaCTe>();
    }
}
