using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoCancSubstituicao : EventoNFeBase
    {
        #region Constructor

        public EventoCancSubstituicao()
        {
            evento = TipoEvento.teCancSubst;
            verAplic = "1.0";
        }

        #endregion Constructor

        #region Properties

        public string verAplic { get; set; }

        public string nProt { get; set; }

        public string xJust { get; set; }

        public string chNFeRef { get; set; }

        #endregion Properties
    }
}