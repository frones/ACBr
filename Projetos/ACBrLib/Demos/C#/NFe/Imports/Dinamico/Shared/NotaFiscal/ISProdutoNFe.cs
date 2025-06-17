namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class ISProdutoNFe
    {
        #region Properties
        
        /// <summary>
        /// Código da Situação Tributária do Imposto Seletivo
        /// </summary>
        public CSTIS CSTIS { get; set; }

        /// <summary>
        /// Código de Classificação Tribútária do Imposto Seletivo
        /// </summary>
        public TipoClassTribIS cClassTribIS { get; set; }

        /// <summary>
        /// Valor da Báse de Cálculo do Imposto Seletivo
        /// </summary>
        public decimal? vBCIS { get; set; }

        /// <summary>
        /// Alíquota do Imposto Seletivo
        /// </summary>
        public decimal? pIS { get; set; }

        /// <summary>
        /// Alíquota especifica por unidade de medida apropriada
        /// </summary>
        public decimal? pISEspec { get; set; }

        /// <summary>
        /// Unidade de Medida Tributável
        /// </summary>
        public string uTrib { get; set; }

        /// <summary>
        /// Quantidade Tributável
        /// </summary>
        public decimal? qTrib { get; set; }

        /// <summary>
        /// VAlor do Imposto Seletivo
        /// </summary>
        public decimal? vIS { get; set; }             

        #endregion Properties
    }
}