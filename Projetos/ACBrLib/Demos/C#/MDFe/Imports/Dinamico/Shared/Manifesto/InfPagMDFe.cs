using System.Collections.Generic;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class InfPagMDFe
    {
        public string xNome { get; set; }

        public string CNPJCPF { get; set; }

        public string idEstrangeiro { get; set; }

        public decimal vContrato { get; set; }

        public IndBoolDFe indAltoDesemp { get; set; }

        public IndPag indPag { get; set; }

        public decimal vAdiant { get; set; }

        public InfBancMDFe InfBanco { get; } = new InfBancMDFe();

        public List<CompMDFe> Comp { get; } = new List<CompMDFe>();

        public List<InfPrazoMDFe> InfPrazo { get; } = new List<InfPrazoMDFe>();
    }
}