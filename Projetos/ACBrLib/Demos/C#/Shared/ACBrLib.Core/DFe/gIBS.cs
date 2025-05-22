using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBS
    {
        #region Properties

        /// <summary>
        /// Percentual do diferimento
        /// </summary>
        public decimal? pDif { get;set; }        

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
        public decimal? pRedAliq { get;set; }

        /// <summary>
        /// Alíquota Efetiva do IBS de competência do Município que será aplicada a Base de Cálculo.
        /// </summary>
        public decimal? pAliqEfet { get; set; }

        #endregion Properties
    }
}