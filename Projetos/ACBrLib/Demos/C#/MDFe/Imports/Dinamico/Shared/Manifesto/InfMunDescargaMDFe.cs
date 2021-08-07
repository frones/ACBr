using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class InfMunDescargaMDFe
    {
        public int cMunDescarga { get; set; }

        public string xMunDescarga { get; set; }

        public List<InfCTeMDFe> InfCTe { get; } = new List<InfCTeMDFe>();

        public List<InfNFeMDFe> InfNFe { get; } = new List<InfNFeMDFe>();

        public List<InfMDFeTransp> InfMDFeTransp { get; } = new List<InfMDFeTransp>();
    }
}