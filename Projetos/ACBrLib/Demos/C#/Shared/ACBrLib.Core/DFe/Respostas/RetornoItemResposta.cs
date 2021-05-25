using System;

namespace ACBrLib.Core.DFe
{
    public sealed class RetornoItemResposta
    {
        #region Properties

        public string Id { get; set; }

        public string tpAmb { get; set; }

        public string verAplic { get; set; }

        public string chDFe { get; set; }

        public DateTime dhRecbto { get; set; }

        public string nProt { get; set; }

        public string digVal { get; set; }

        public int cStat { get; set; }

        public string xMotivo { get; set; }

        public string XML { get; set; }

        #endregion Properties
    }
}