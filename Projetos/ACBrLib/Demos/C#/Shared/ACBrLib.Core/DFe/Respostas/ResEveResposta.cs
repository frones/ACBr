using System;

namespace ACBrLib.Core.DFe
{
    public sealed class ResEveResposta<TEvento> : DistribuicaoDFeItemResposta where TEvento : Enum
    {
        #region Properties

        public string CNPJCPF { get; set; }

        public DateTime dhEvento { get; set; }

        public TEvento tpEvento { get; set; }

        public string xEvento { get; set; }

        public int nSeqEvento { get; set; }

        public int cOrgao { get; set; }

        public DateTime dhRecbto { get; set; }

        public string nProt { get; set; }

        #endregion Properties
    }
}