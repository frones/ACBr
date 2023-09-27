using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public abstract class EventoMDFeBase : EventoBase<TipoEventoMDFe>
    {
        #region Properties

        public string chMDFe { get; set; }

        public string nProt { get; set; }

        public string CNPJCPF { get; set; }

        #endregion Properties
    }
}