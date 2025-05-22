using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSMun:gIBS
    {
        #region Properties

        /// <summary>
        /// Alíquota do IBS de competência do Município
        /// </summary>
        public decimal pIBSMun { get; set; }

        /// <summary>
        /// Valor do IBS de competência do Município
        /// </summary>
        public decimal vIBSMun { get; set; }

        /// <summary>
        /// Valor da CBS Bruto na operação
        /// </summary>
        public decimal? vCBSOp { get; set; }       

        #endregion Properties
    }
}