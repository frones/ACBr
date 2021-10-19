using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestDesconhecimento : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestDesconhecimento()
        {
            tpEvento = TipoEventoNFe.teManifDestDesconhecimento;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}