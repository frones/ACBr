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
    }
}