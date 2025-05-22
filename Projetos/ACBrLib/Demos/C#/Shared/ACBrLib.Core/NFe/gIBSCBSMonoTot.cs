using ACBrLib.Core.DFe;
using System.Runtime.CompilerServices;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSCBSMonoTot
    {
        #region Properties

        /// <summary>
        /// Total do IBS monofásico
        /// </summary>
        public decimal vIBSMono { get; set; }

        /// <summary>
        /// Total da CBS monofásica
        /// </summary>
        public decimal vCBSMono { get; set; }

        /// <summary>
        /// Total da IBS sujeita a retenção
        /// </summary>
        public decimal vIBSMonoReten { get; set; }

        /// <summary>
        /// Total da CBS sujeita a rentenção
        /// </summary>
        public decimal vCBSMonoReten { get; set; }

        /// <summary>
        /// Total do IBS monofásico retido anteriormente
        /// </summary>
        public decimal vIBSMonoRet { get; set; }

        /// <summary>
        /// Total da CBS monófásica retida anteriormente
        /// </summary>
        public decimal vCBSMonoRet { get; set; }  

        #endregion Properties
    }
}