using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestConfirmacao : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestConfirmacao()
        {
            evento = TipoEvento.teManifDestConfirmacao;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}