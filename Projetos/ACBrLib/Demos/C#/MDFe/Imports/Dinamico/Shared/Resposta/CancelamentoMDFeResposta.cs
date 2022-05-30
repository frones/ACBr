using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class CancelamentoMDFeResposta : CancelamentoRespostaBase<CancelamentoMDFeResposta, TipoEventoMDFe>
    {
        #region Properties

        public string ChMDFe { get; set; }

        public string CNPJCPF { get; set; }

        #endregion Properties
    }
}