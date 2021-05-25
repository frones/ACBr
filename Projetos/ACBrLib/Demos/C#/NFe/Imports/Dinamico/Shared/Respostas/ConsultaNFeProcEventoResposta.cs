using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class ConsultaNFeProcEventoResposta
    {
        #region Properties

        public int ID { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public string cOrgao { get; set; }

        public string CNPJ { get; set; }

        public string chNFe { get; set; }

        public DateTime dhEvento { get; set; }

        public TipoEvento tpEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string verEvento { get; set; }

        public ConsultaNFeDetEventoResposta DetEvento { get; set; }

        #endregion Properties
    }
}