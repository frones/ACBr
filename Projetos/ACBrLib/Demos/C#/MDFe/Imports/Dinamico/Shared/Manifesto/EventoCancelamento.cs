using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class EventoCancelamento : EventoMDFeBase
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