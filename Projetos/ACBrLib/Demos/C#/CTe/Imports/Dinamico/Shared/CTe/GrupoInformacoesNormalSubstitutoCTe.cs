using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class GrupoInformacoesNormalSubstitutoCTe
    {
        public InfCargaCTe infCarga { get; set; }

        public InfDocCTe infDoc { get; set; }

        public DocAntCTe docAnt { get; set; }

        public InfModalCTe infModal { get; set; }

        public VeicNovosCTe veicNovos { get; set; }

        public CobrCTe cobr { get; set; }

        public InfCteSubCTe infCTeSub { get; set; }

        public InfGlobalizadoCTe infGlobalizado { get; set; }

        public InfServVincCTe infServVinc { get; set; }
    }
}
