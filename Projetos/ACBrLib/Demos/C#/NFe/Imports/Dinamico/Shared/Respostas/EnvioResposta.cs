using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EnvioResposta : DFeRespostaBase
    {
        #region Properties

        public int TMed { get; set; }

        public string NRec { get; set; }

        public string NProt { get; set; }

        public RetornoItemResposta ItemResposta { get; set; }

        #endregion Properties
    }
}