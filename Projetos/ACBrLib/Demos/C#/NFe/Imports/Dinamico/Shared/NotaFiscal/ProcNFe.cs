using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class ProcNFe
    {
        public TipoAmbiente tpAmb { get; set; }

        public string verAplic { get; set; }

        public string chNFe { get; set; }

        public DateTime dhRecbto { get; set; }

        public string nProt { get; set; }

        public string digVal { get; set; }

        public int cStat { get; set; }

        public string xMotivo { get; set; }
    }
}