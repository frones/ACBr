using System.Collections.Generic;

namespace ACBrLib.NFe
{
    public class CombustivelNFe
    {
        #region Constructors

        public CombustivelNFe()
        {
            CIDE = new CIDENFe();
            Encerrante = new EncerranteNFe();            
        }

        #endregion Constructors

        #region Properties

        public int cProdANP { get; set; }

        public decimal pMixGN { get; set; }

        public string CODIF { get; set; }

        public decimal qTemp { get; set; }

        public string UFCons { get; set; }

        public string descANP { get; set; }

        public decimal pGLP { get; set; }

        public decimal pGNn { get; set; }

        public decimal pGNi { get; set; }

        public decimal vPart { get; set; }

        public decimal pBio { get; set; }

        public CIDENFe CIDE { get; }

        public EncerranteNFe Encerrante { get; }

        public List<OrigCombNFe> OrigComb { get; } = new List<OrigCombNFe>();

        #endregion Properties
    }
}