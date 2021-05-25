using System;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Dados da Cobrança - Duplicata
    /// </summary>
    public class DuplicataNFe
    {
        #region Properties

        /// <summary>
        /// Número da Duplicata
        /// </summary>
        public string nDup { get; set; }

        /// <summary>
        /// Data de vencimento
        /// </summary>
        public DateTime dVenc { get; set; }

        /// <summary>
        /// Valor da duplicata
        /// </summary>
        public decimal vDup { get; set; }

        #endregion Properties
    }
}