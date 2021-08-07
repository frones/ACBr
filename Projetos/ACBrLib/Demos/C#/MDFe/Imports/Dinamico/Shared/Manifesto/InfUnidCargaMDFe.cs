using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class InfUnidCargaMDFe
    {
        public UnidCarga tpUnidCarga { get; set; }

        public string idUnidCarga { get; set; }

        public decimal qtdRat { get; set; }

        public List<LacreMDFe> lacUnidCarga { get; } = new List<LacreMDFe>();
    }
}