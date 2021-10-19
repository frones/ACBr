using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoManifDestCiencia : EventoNFeBase
    {
        #region Constructors

        public EventoManifDestCiencia()
        {
            tpEvento = TipoEventoNFe.teManifDestCiencia;
            cOrgao = 91;
        }

        #endregion Constructors
    }
}