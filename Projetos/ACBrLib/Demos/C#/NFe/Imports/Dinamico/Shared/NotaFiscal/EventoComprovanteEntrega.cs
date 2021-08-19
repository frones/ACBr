using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoComprovanteEntrega : EventoNFeBase
    {
        #region Constructor

        public EventoComprovanteEntrega()
        {
            tpEvento = TipoEvento.teComprEntregaNFe;
            verAplic = "1.0";
        }

        #endregion Constructor

        #region Properties

        public string cOrgaoAutor { get; set; }

        public TipoAutor tpAutor { get; set; }

        public string verAplic { get; set; }

        public string nProtEvento { get; set; }

        #endregion Properties
    }
}