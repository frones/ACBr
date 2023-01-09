using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ComplementoCTe
    {
        public string xCaracAd { get; set; }

        public string xCaracSer { get; set; }

        public string xEmi { get; set; }

        public FluxoCTe Fluxo { get; set; }

        public TipoPeriodoCTe semData { get; set; }

        public EntregaCTe comData { get; set; }

        public TipoHorarioCTe noPeriodo { get; set; }

        public TipoHorarioCTe semHora { get; set; }

        public EntregaCTe comHora { get; set; }

        public EntregaCTe noInter { get; set; }

        public string origCalc { get; set; }

        public string destCalc { get; set; }

        public string xObs { get; set; }

        public ObsContCTe ObsCont { get; set; }

        public ObsFiscoCTe ObsFisco { get; set; }
    }
}
