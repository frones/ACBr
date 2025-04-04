using System;
using System.Collections.Generic;
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

        public TipoEventoNFe tpEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string verEvento { get; set; }

        public string Xml { get; set; }

        public ConsultaNFeDetEventoResposta DetEvento { get; set; }

        public List<ConsultaNFeRetEventoResposta> RetEvento { get; } = new List<ConsultaNFeRetEventoResposta>();

        #endregion Properties
    }
}