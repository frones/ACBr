namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class IIProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Valor BC do Imposto de Importação
        /// </summary>
        public decimal? vBC { get; set; }

        /// <summary>
        /// Valor despesas aduaneiras
        /// </summary>
        public decimal? vDespAdu { get; set; }

        /// <summary>
        /// Valor Imposto de Importação
        /// </summary>
        public decimal? vII { get; set; }

        /// <summary>
        /// Valor Imposto sobre Operações Financeiras
        /// </summary>
        public decimal? vIOF { get; set; }

        #endregion Properties
    }
}