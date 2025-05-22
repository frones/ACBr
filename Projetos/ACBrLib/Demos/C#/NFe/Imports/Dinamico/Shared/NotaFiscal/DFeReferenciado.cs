using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class DFeReferenciado
    {
        #region Properties

        /// <summary>
        /// Chave de acesso do DF-e referenciado
        /// </summary>
        public string chaveAcesso { get; set; }

        /// <summary>
        /// Número do item do documento referenciado
        /// </summary>
        public int nItem { get; set; }
       
        #endregion Properties
    }
}