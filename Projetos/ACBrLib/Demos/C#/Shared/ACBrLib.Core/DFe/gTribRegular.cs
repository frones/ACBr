using ACBrLib.Core.DFe;
using System.Net;
using System.Runtime.CompilerServices;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gTribRegular
    {
        #region Properties

        /// <summary>
        /// Código de Situação Tributária do IBS e CBS
        /// </summary>
        public CSTIBSCBS CSTReg { get; set; }

        /// <summary>
        /// Código de Classificação Tributária do IBS e CBS
        /// </summary>
        public cClassTribIBSCBS cClassTribReg { get; set; }

        /// <summary>
        /// Valor da alíquota do IBS da UF
        /// </summary>
        public decimal pAliqEfetRegIBSUF { get; set; }

        /// <summary>
        /// Valor do Tributo do IBS da UF
        /// </summary>
        public decimal vTribRegIBSUF { get; set; }

        /// <summary>
        /// Valor da alíquota do IBS do Município
        /// </summary>
        public decimal pAliqEfetRegIBSMun { get; set; }

        /// <summary>
        /// Valor do Tributo do IBS do Município
        /// </summary>
        public decimal vTribRegIBSMun { get;set; }

        /// <summary>
        /// Valor da alíquota  da CBS
        /// </summary>
        public decimal pAliqEfetRegCBS { get; set; }

        /// <summary>
        /// Valor do Tributo da CBS
        /// </summary>
        public decimal vTribRegCBS { get; set; }

        #endregion Properties
    }
}