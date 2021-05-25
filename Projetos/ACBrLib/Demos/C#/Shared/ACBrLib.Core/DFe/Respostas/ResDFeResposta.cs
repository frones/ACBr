using System;
using ACBrLib.Core.NFe;

namespace ACBrLib.Core.DFe
{
    public sealed class ResDFeResposta : DistribuicaoDFeItemResposta
    {
        #region Properties

        public string CNPJCPF { get; set; }

        public string xNome { get; set; }

        public string IE { get; set; }

        public DateTime dhEmi { get; set; }

        public TipoNFe tpNF { get; set; }

        public decimal vNF { get; set; }

        public string digVal { get; set; }

        public DateTime dhRecbto { get; set; }

        public SituacaoDFe cSitNFe { get; set; }

        public string nProt { get; set; }

        #endregion Properties
    }
}