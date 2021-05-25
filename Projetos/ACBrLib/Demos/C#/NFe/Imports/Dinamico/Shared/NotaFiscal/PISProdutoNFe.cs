namespace ACBrLib.NFe
{
    /// <summary>
    /// PIS
    /// </summary>
    public class PISProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Código de Situação Tributária do PIS
        /// <para>01=Operação Tributável (base de cálculo = valor da operação alíquota normal(cumulativo/não cumulativo));</para>
        /// <para>02=Operação Tributável(base de cálculo = valor da operação(alíquota diferenciada));</para>
        /// </summary>
        public CSTPIS CST { get; set; }

        /// <summary>
        /// Valor da Base de Cálculo do PIS
        /// </summary>
        public decimal? vBC { get; set; }

        /// <summary>
        /// Alíquota do PIS (em percentual)
        /// </summary>
        public decimal? pPIS { get; set; }

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