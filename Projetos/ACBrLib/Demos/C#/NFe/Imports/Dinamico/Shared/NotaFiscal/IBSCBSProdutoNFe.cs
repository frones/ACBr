using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class IBSCBSProdutoNFe
    {
        #region Constructors
        public IBSCBSProdutoNFe()
        {
            gIBSCBS = new gIBSCBS();
            gIBSCBSMono = new gIBSCBSMono();
            gTransfCred = new gTransfCred();
            gCredPresIBSZFM = new gCredPresIBSZFM();

        }
        #endregion Constructors
        #region Properties

        /// <summary>
        /// Código de Classificação Tributária do IBS e CBS
        /// </summary>
        public CSTIBSCBS CST { get; set; }

        /// <summary>
        /// Código de Classificação Tributária do IBS e CBS
        /// </summary>
        public cClassTribIBSCBS cClassTrib { get; set; }  
        
        /// <summary>
        /// Grupo de Informações do IBS e da CBS
        /// </summary>
        public gIBSCBS gIBSCBS { get; }

        public gIBSCBSMono gIBSCBSMono { get; }

        public gTransfCred gTransfCred { get; }      
        
        public gCredPresIBSZFM gCredPresIBSZFM { get; }

        #endregion Properties
    }
}