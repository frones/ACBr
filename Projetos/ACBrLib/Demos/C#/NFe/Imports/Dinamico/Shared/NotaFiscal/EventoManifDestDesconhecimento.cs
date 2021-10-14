using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestDesconhecimento : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestDesconhecimento()
        {
            evento = TipoEvento.teManifDestDesconhecimento;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}