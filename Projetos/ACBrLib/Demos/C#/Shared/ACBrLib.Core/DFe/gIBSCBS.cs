using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto de Importação
    /// </summary>
    public class gIBSCBS
    {
        #region Constructors

        public gIBSCBS()
        {
            gIBSUF = new gIBSUF();
            gIBSMun = new gIBSMun();
            gCBS = new gCBS();
            gTribRegular = new gTribRegular();
            gIBSCredPres = new gCredPres();
            gCBSCredPres = new gCredPres();
        }

        #endregion Constructors
        #region Properties

        /// <summary>
        /// Base de cálculo do IBS e CBS
        /// </summary>
        public decimal vBC { get; set; }

        /// <summary>
        /// Grupo de informações do IBS para a UF.
        /// </summary>
        public gIBSUF gIBSUF { get; }

        /// <summary>
        /// Grupo de informações do IBS para o Município.
        /// </summary>
        public gIBSMun gIBSMun { get; } 

        public gCBS gCBS { get; }

        public gTribRegular gTribRegular { get; }

        public gCredPres gIBSCredPres { get; }

        public gCredPres gCBSCredPres { get; }


        #endregion Properties
    }
}