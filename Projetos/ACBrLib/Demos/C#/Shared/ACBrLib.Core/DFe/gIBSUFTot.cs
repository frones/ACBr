using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSUFTot
    {
        #region Properties
        
        /// <summary>
        /// Valor total do diferimento
        /// </summary>
        public decimal vDif { get; set; }

        /// <summary>
        /// Valor total de devolução de tributos
        /// </summary>
        public decimal vDevTrib { get; set; }

        /// <summary>
        /// Valor total do IBS da UF
        /// </summary>
        public decimal vIBSUF { get; set; }

        #endregion Properties
    }
}