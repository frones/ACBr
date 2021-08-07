using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class InfUnidTranspMDFe
    {
        public UnidTransp tpUnidTransp { get; set; }

        public string idUnidTransp { get; set; }

        public decimal qtdRat { get; set; }

        public List<LacreMDFe> lacUnidTransp { get; } = new List<LacreMDFe>();

        public List<InfUnidCargaMDFe> infUnidCarga { get; } = new List<InfUnidCargaMDFe>();
    }
}