using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoCancelamento : EventoBase
    {
        #region Constructor

        public EventoCancelamento()
        {
            tpEvento = TipoEvento.teCancelamento;
        }

        #endregion Constructor

        #region Properties

        public string nProt { get; set; }

        public string xJust { get; set; }

        #endregion Properties
    }
}