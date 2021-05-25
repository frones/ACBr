namespace ACBrLib.NFe
{
    /// <summary>
    /// PIS ST
    /// </summary>
    public class PISSTProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Valor da Base de Cálculo do PIS
        /// </summary>
        public decimal? vBC { get; set; }

        /// <summary>
        /// Alíquota do PIS (em percentual)
        /// </summary>
        public decimal? pPis { get; set; }

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        public decimal? qBCProd { get; set; }

        /// <summary>
        /// Alíquota do PIS (em reais)
        /// </summary>
        public decimal? vAliqProd { get; set; }

        /// <summary>
        /// Valor do PIS
        /// </summary>
        public decimal? vPIS { get; set; }

        #endregion Properties
    }
}