using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class GrupoInformacoesNormalSubstitutoCTe
    {
        public InfCargaCTe infCarga { get; set; } = new InfCargaCTe();

        public InfDocCTe infDoc { get; set; } = new InfDocCTe();

        public DocAntCTe docAnt { get; set; } = new DocAntCTe();

        public InfModalCTe infModal { get; set; } = new InfModalCTe();

        public VeicNovosCTe veicNovos { get; set; } = new  VeicNovosCTe();

        public CobrCTe cobr { get; set; } = new CobrCTe();

        public InfCteSubCTe infCTeSub { get; set; } = new InfCteSubCTe();

        public InfGlobalizadoCTe infGlobalizado { get; set; } = new InfGlobalizadoCTe();

        public InfServVincCTe infServVinc { get; set; } = new InfServVincCTe();

        public List<InfGTVe> infGTVe { get; } = new List<InfGTVe>();

        public InfServico infServico { get; set; } = new InfServico();
    }
}
