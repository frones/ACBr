namespace ACBrLib.Core.DFe
{
    public abstract class DistribuicaoDFeItemResposta
    {
        #region Properties

        public string NSU { get; set; }

        public string XML { get; set; }

        public string Arquivo { get; set; }

        public DistSchema schema { get; set; }

        public string chDFe { get; set; }

        #endregion Properties
    }
}