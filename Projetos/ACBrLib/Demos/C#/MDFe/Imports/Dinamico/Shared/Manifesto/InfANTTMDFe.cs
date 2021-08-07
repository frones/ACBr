using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class InfANTTMDFe
    {
        public string RNTRC { get; set; }

        public List<InfCIOTMDFe> InfCIOT { get; } = new List<InfCIOTMDFe>();

        public ValePedMDFe valePed { get; } = new ValePedMDFe();

        public List<InfContratanteMDFe> InfContratante { get; } = new List<InfContratanteMDFe>();

        public List<InfPagMDFe> InfPag { get; } = new List<InfPagMDFe>();
    }
}