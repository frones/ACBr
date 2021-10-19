using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class CancelamentoNFeResposta : CancelamentoRespostaBase<CancelamentoNFeResposta, TipoEventoNFe>
    {
        #region Properties

        public string chNFe { get; set; }

        #endregion Properties
    }
}