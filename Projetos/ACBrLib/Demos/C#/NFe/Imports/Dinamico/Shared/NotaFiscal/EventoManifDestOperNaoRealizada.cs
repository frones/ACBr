using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestOperNaoRealizada : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestOperNaoRealizada()
        {
            evento = TipoEvento.teManifDestOperNaoRealizada;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}