using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestOperNaoRealizada : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestOperNaoRealizada()
        {
            tpEvento = TipoEventoNFe.teManifDestOperNaoRealizada;
            cOrgao = 91;
        }

        #endregion Constructors

        #region Properties

        public string xJust { get; set; }

        #endregion Properties
    }
}