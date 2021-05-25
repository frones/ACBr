using System;

namespace ACBrLib.NFe
{
    public class ISSQNtotNFe
    {
        public decimal? vServ { get; set; }

        public decimal? vBC { get; set; }

        public decimal? vISS { get; set; }

        public decimal? vPIS { get; set; }

        public decimal? vCOFINS { get; set; }

        public DateTime? dCompet { get; set; }

        public decimal? vDeducao { get; set; }

        public decimal? vOutro { get; set; }

        public decimal? vDescIncond { get; set; }

        public decimal? vDescCond { get; set; }

        public decimal? vISSRet { get; set; }

        public RegTribISSQN cRegTrib { get; set; } = RegTribISSQN.RTISSNenhum;
    }
}