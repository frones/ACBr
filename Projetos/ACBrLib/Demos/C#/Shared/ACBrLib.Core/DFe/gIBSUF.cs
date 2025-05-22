using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSUF:gIBS
    {
        #region Properties

        /// <summary>
        /// Alíquota do IBS de competência das UF
        /// </summary>
        public decimal pIBSUF { get; set; }

        /// <summary>
        /// Valor do IBS de competência da UF
        /// </summary>
        public decimal vIBSUF { get; set; }       

        #endregion Properties
    }
}