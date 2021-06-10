using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoCancComprovanteEntrega : EventoBase
    {
        #region Constructor

        public EventoCancComprovanteEntrega()
        {
            tpEvento = TipoEvento.teCancComprEntregaNFe;
        }

        #endregion Constructor

        #region Properties

        public string cOrgaoAutor { get; set; }

        public TipoAutor tpAutor { get; set; }

        public string verAplic { get; set; }

        public DateTime dhEntrega { get; set; }

        public string nDoc { get; set; }

        public string xNome { get; set; }

        public string latGPS { get; set; }

        public string longGPS { get; set; }

        public string hashComprovante { get; set; }

        public DateTime dhHashComprovante { get; set; }

        #endregion Properties
    }
}