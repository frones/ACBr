using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoCancelamento : EventoNFeBase
    {
        #region Constructor

        public EventoCancelamento()
        {
            tpEvento = TipoEventoNFe.teCancelamento;
        }

        #endregion Constructor

        #region Properties

        public string nProt { get; set; }

        public string xJust { get; set; }

        #endregion Properties
    }
}