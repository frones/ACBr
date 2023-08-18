using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class IdentificacaoCTe
    {
        public string cUF { get; set; }

        public int cCT { get; set; }

        public int CFOP { get; set; }

        public string NatOp { get; set; }

        public int mod { get; set; }

        public int serie { get; set; }

        public int nCT { get; set; }

        public DateTime dhEmi { get; set; }

        public TipoDACTE tpImp { get; set; }

        public TipoEmissao tpEmis { get; set; }

        public int cDV { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public TipoCTe tpCte { get; set; }

        public ProcessoEmissao ProcEmi { get; set; }

        public string verProc { get; set; }

        public int indGlobalizado { get; set; }

        public int cMunEnv { get; set; }

        public string xMunEnv { get; set; }

        public string UFEnv { get; set; }

        public ModalCTe modal { get; set; }

        public TipoServicoCTe tpServ { get; set; }

        public int cMunIni { get; set; }

        public string xMunIni { get; set; }

        public string UFIni { get; set; }

        public int cMunFim { get; set; }

        public string xMunFim { get; set; }

        public string UFFim { get; set; }

        public int retira { get; set; }

        public string xDetRetira { get; set; }

        public IndicadorTomadorCTe indIEToma { get; set; }

        [Obsolete("Descontinuado: Para o correto preenchimento, use Tomador3")]
        public int toma { get; set; }

        public List<InfPercursoCTe> infPercurso { get; set; } = new List<InfPercursoCTe>();

        public DateTime dhCont { get; set; }

        public string xJust { get; set; }
    }
}
