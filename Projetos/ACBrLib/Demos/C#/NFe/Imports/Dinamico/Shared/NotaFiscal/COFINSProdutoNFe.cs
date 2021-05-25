namespace ACBrLib.NFe
{
    /// <summary>
    /// COFINS
    /// </summary>
    public class COFINSProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Código de Situação Tributária da COFINS
        /// <para>01=Operação Tributável (base de cálculo = valor da operação alíquota normal(cumulativo/não cumulativo));</para>
        /// <para>02=Operação Tributável(base de cálculo = valor da operação (alíquota diferenciada));</para>
        /// </summary>
        public CSTCofins CST { get; set; }

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