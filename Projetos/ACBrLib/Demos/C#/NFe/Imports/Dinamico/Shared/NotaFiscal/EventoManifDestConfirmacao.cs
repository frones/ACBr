using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestConfirmacao : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestConfirmacao()
        {
            tpEvento = TipoEvento.teManifDestConfirmacao;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}