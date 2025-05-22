using ACBrLib.Core.DFe;
using System.Net;
using System.Runtime.CompilerServices;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gCBSTot
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
        /// Valor total da CBS
        /// </summary>
        public decimal vCBS { get; set; }

        /// <summary>
        /// Valor total do crédito presumido
        /// </summary>
        public decimal vCredPres { get; set; }

        /// <summary>
        /// Valor total do crédito presumido em condição suspensiva
        /// </summary>
        public decimal vCredPresCondSus { get; set; } 

        #endregion Properties
    }
}