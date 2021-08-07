using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class IdeMDFe
    {
        public TpEmitenteMDFe tpEmit { get; set; }

        public string mod { get; set; } = "58";

        public int serie { get; set; }

        public int nMDF { get; set; }

        public int cMDF { get; set; }

        public ModalMDFe modal { get; set; }

        public DateTime dhEmi { get; set; }

        public TipoEmissao tpEmis { get; set; }

        public ProcessoEmissao procEmi { get; set; }

        public string verProc { get; set; }

        public string UFIni { get; set; }

        public string UFFim { get; set; }

        public TransportadorMDFe tpTransp { get; set; }

        public DateTime? dhIniViagem { get; set; }

        public IndBoolDFe indCanalVerde { get; set; }

        public IndBoolDFe indCarregaPosterior { get; set; }

        public int cUF { get; set; }
    }
}