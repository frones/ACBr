using ACBrLib.Core.DFe;
using System.Net;
using System.Runtime.CompilerServices;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gCBS
    {
        #region Properties

        /// <summary>
        /// Grupo de Informações da CBS
        /// </summary>
        public decimal pCBS { get; set; }

        /// <summary>
        /// Valor da CBS
        /// </summary>
        public decimal vCBS { get; set; }

        /// <summary>
        /// Percentual do diferimento
        /// </summary>
        public decimal? pDif { get; set; }

        /// <summary>
        /// Valor do diferimento
        /// </summary>
        public decimal? vDif { get; set; }

        /// <summary>
        /// Valor do tributo devolvido
        /// </summary>
        public decimal? vDevTrib { get; set; }

        /// <summary>
        /// Percentual da redução de alíquota
        /// </summary>
        public decimal? pRedAliq { get; set; }

        /// <summary>
        /// Alíquota efetiva da CBS que será aplicada a base de cálculo
        /// </summary>
        public decimal? pAliqEfet { get; set; }

        #endregion Properties
    }
}