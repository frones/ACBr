using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoCancSubstituicao : EventoNFeBase
    {
        #region Constructor

        public EventoCancSubstituicao()
        {
            tpEvento = TipoEventoNFe.teCancSubst;
            verAplic = "1.0";
        }

        #endregion Constructor

        #region Properties
        public int cOrgaoAutor { get; set; }

        public string verAplic { get; set; }

        public string nProt { get; set; }

        public string xJust { get; set; }

        public string chNFeRef { get; set; }

        #endregion Properties
    }
}