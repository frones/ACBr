namespace ACBrLib.NFe
{
    /// <summary>
    /// Dados da Cobrança - Fatura
    /// </summary>
    public class FaturaNFe
    {
        #region Properties

        /// <summary>
        /// Número da Fatura
        /// </summary>
        public string nFat { get; set; }

        /// <summary>
        /// Valor Original da Fatura
        /// </summary>
        public decimal? vOrig { get; set; }

        /// <summary>
        /// Valor do desconto
        /// </summary>
        public decimal? vDesc { get; set; }

        /// <summary>
        /// Valor Líquido da Fatura
        /// </summary>
        public decimal? vLiq { get; set; }

        #endregion Properties
    }
}