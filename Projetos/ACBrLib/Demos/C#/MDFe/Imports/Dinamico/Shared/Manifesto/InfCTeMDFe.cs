using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class InfCTeMDFe
    {
        public string chCTe { get; set; }

        public string SegCodBarra { get; set; }

        public string indReentrega { get; set; }

        public List<PeriMDFe> Peri { get; } = new List<PeriMDFe>();

        public List<InfEntregaParcialMDFe> InfEntregaParcial { get; } = new List<InfEntregaParcialMDFe>();

        public List<InfUnidTranspMDFe> InfUnidTransp { get; } = new List<InfUnidTranspMDFe>();
    }
}