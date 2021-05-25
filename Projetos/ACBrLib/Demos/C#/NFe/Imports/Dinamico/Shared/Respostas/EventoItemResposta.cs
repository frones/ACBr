using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoItemResposta
    {
        #region Properties

        public TipoAmbiente tpAmb { get; set; }

        public string VerAplic { get; set; }

        public int CStat { get; set; }

        public string XMotivo { get; set; }

        public string chNFe { get; set; }

        public string nProt { get; set; }

        public string arquivo { get; set; }

        public string digVal { get; set; }

        public string Id { get; set; }

        public int cOrgao { get; set; }

        public DateTime dhRegEvento { get; set; }

        public TipoEvento tpEvento { get; set; }

        public string xEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string CNPJDest { get; set; }

        public string emailDest { get; set; }

        public string XML { get; set; }

        #endregion Properties
    }
}