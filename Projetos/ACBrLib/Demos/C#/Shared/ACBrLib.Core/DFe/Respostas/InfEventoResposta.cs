using System;

namespace ACBrLib.Core.DFe
{
    public sealed class InfEventoResposta : DistribuicaoDFeItemResposta
    {
        #region Properties

        public string Id { get; set; }

        public string VerAplic { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public int cOrgao { get; set; }

        public int CStat { get; set; }

        public string CNPJDest { get; set; }

        public int cOrgaoAutor { get; set; }

        public TipoEvento tpEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string xEvento { get; set; }

        public string xMotivo { get; set; }

        public DateTime dhRegEvento { get; set; }

        public string emailDest { get; set; }

        public string nProt { get; set; }

        #endregion Properties
    }
}