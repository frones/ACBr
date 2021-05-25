using System;

namespace ACBrLib.Core.DFe
{
    public sealed class ProcEveResposta : DistribuicaoDFeItemResposta
    {
        #region Properties

        public int cOrgao { get; set; }

        public string CNPJ { get; set; }

        public string Id { get; set; }

        public DateTime dhEvento { get; set; }

        public int nSeqEvento { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public string tpEvento { get; set; }

        public string verEvento { get; set; }

        public string descEvento { get; set; }

        public string xJust { get; set; }

        public string xMotivo { get; set; }

        public string EmiCNPJ { get; set; }

        public string EmiIE { get; set; }

        public string EmixNome { get; set; }

        public string cteNProt { get; set; }

        public string cteChvCte { get; set; }

        public DateTime cteDhemi { get; set; }

        public string cteModal { get; set; }

        public DateTime cteDhRebcto { get; set; }

        #endregion Properties
    }
}