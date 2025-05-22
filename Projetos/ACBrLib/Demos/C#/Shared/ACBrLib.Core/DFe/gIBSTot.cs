using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSTot
    {
        #region Constructors
        public gIBSTot()
        {
            gIBSMun = new gIBSMunTot();
            gIBSUF = new gIBSUFTot();
        }
        #endregion Constructors
        #region Properties

        /// <summary>
        /// Valot total do IBS
        /// </summary>
        public decimal vIBS { get; set; }

        /// <summary>
        /// Valor total do crédito presumido
        /// </summary>
        public decimal vCredPres { get; set; }

        /// <summary>
        /// Valor total do crédito presumido em condição suspensiva
        /// </summary>
        public decimal vCredPresCondSus { get; set; }

        /// <summary>
        /// Grupo total do IBS da UF
        /// </summary>
        public gIBSUFTot gIBSUF { get; }

        /// <summary>
        /// Grupo total do IBS do Município
        /// </summary>
        public gIBSMunTot gIBSMun { get; }

        #endregion Properties
    }
}