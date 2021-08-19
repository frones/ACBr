using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestOperNaoRealizada : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestOperNaoRealizada()
        {
            tpEvento = TipoEvento.teManifDestOperNaoRealizada;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}