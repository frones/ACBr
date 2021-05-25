namespace ACBrLib.NFe
{
    /// <summary>
    /// COFINS ST
    /// </summary>
    public class COFINSSTProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Valor da Base de Cálculo da COFINS
        /// </summary>
        public decimal? vBC { get; set; }

        /// <summary>
        /// Alíquota da COFINS (em percentual)
        /// </summary>
        public decimal? pCOFINS { get; set; }

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        public decimal? qBCProd { get; set; }

        /// <summary>
        /// Alíquota da COFINS (em reais)
        /// </summary>
        public decimal? vAliqProd { get; set; }

        /// <summary>
        /// Valor da COFINS
        /// </summary>
        public decimal? vCOFINS { get; set; }

        #endregion Properties
    }
}