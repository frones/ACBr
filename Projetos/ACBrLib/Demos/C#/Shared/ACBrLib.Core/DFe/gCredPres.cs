using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gCredPres
    {
        #region Properties

        /// <summary>
        /// Código de Classificação do Crédito Presumido
        /// </summary>
        public int cCredPres { get; set; }

        /// <summary>
        /// Percentual do Crédito Presumido
        /// </summary>
        public decimal pCredPres { get; set; }

        /// <summary>
        /// Valor do Crédito Presumido
        /// </summary>
        public decimal vCredPres { get; set; }

        /// <summary>
        /// Valor do Crédito Presumido em condição suspensiva
        /// </summary>
        //public decimal vCredPresCondSus { get; set; } 

        #endregion Properties
    }
}