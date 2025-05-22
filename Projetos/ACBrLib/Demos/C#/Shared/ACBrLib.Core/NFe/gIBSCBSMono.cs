using ACBrLib.Core.DFe;
using System.Runtime.CompilerServices;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSCBSMono
    {
        #region Properties

        /// <summary>
        /// Quantidade tributada na monofasia
        /// </summary>
        public decimal qBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do IBS
        /// </summary>
        public decimal adRemIBS { get; set; }

        /// <summary>
        /// Alíquota ad rem do CBS
        /// </summary>
        public decimal adRemCBS { get; set; }

        /// <summary>
        /// Valor do IBS monofásico
        /// </summary>
        public decimal vIBSMono { get; set; }

        /// <summary>
        /// Valor da CBS monofásica
        /// </summary>
        public decimal vCBSMono { get; set; }

        /// <summary>
        /// Quantidade tributada sujeita à retenção na monofasia
        /// </summary>
        public decimal qBCMonoReten { get; set; }

        /// <summary>
        /// Alíquota ad rem do IBS sujeito a retenção
        /// </summary>
        public decimal adRemIBSReten { get; set; }

        /// <summary>
        /// Valor do IBS monofásico sujeito a retenção
        /// </summary>
        public decimal vIBSMonoReten { get; set; } 

        /// <summary>
        /// Alíquota ad rem da CBS sujeito a retenção
        /// </summary>
        public decimal adRemCBSReten { get; set; }

        /// <summary>
        /// Valor da CBS monofásica sujeita a retenção
        /// </summary>
        public decimal vCBSMonoReten { get; set; }

        /// <summary>
        /// Quantidade tributada retina anteriormente
        /// </summary>
        public decimal qBCMonoRet { get; set; }

        /// <summary>
        /// Alíquota ad rem do IBS retido anteriormente
        /// </summary>
        public decimal adRemIBSRet { get; set; }

        /// <summary>
        /// Valor do IBS retido anteriormente
        /// </summary>
        public decimal vIBSMonoRet { get; set; }

        /// <summary>
        /// Alíquota ad rem da CBS retida anteriormente
        /// </summary>
        public decimal adRemCBSRet { get; set; }

        /// <summary>
        /// Valor da CBS retida anteriormente
        /// </summary>
        public decimal vCBSMonoRet { get; set; }

        /// <summary>
        /// Percentual do diferimento do imposto monofásico
        /// </summary>
        public decimal pDifIBS { get; set; }

        /// <summary>
        /// Valor do IBS monofásico diferido
        /// </summary>
        public decimal vIBSMonoDif { get; set; }

        /// <summary>
        /// Percentual do diferimento do imposto monofásico
        /// </summary>
        public decimal pDifCBS { get; set; }

        /// <summary>
        /// Valor da CBS Monofásica diferida
        /// </summary>
        public decimal vCBSMonoDif { get; set; }

        /// <summary>
        /// Total de IBS Monofásico
        /// </summary>
        public decimal vTotIBSMonoItem { get; set; }

        /// <summary>
        /// Total da CBS Monofásica
        /// </summary>
        public decimal vTotCBSMonoItem { get; set; }   

        #endregion Properties
    }
}