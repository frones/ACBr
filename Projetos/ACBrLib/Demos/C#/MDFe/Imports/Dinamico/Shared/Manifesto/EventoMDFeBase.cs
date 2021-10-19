using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public abstract class EventoMDFeBase : EventoBase<TipoEventoMDFe>
    {
        #region Properties

        public string chMDFe { get; set; }

        #endregion Properties
    }
}