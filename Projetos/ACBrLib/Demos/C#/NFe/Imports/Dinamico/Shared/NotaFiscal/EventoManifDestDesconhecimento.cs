using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestDesconhecimento : EventoBase
    {
        #region Constructors

        public EventoManifDestDesconhecimento()
        {
            tpEvento = TipoEvento.teManifDestDesconhecimento;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}