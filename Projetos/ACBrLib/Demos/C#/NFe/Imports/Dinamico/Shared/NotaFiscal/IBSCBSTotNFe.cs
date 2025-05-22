namespace ACBrLib.NFe
{
    public class IBSCBSTotNFe
    {
        #region Constructors
        public IBSCBSTotNFe()
        {
            gIBS = new gIBSTot();
            gCBS = new gCBSTot();
            gMono = new gIBSCBSMonoTot();

        }
        #endregion Constructors
        #region Properties

        /// <summary>
        /// Valor total da BC do IBS e da CBS
        /// </summary>
        public decimal vBCIBSCBS { get; set; }

        /// <summary>
        /// Grupo total do IBS
        /// </summary>
        public gIBSTot gIBS { get; }

        /// <summary>
        /// Grupo total do CBS
        /// </summary>
        public gCBSTot gCBS { get; }

        public gIBSCBSMonoTot gMono { get; }
       

        #endregion Properties
    }
}