using System.Collections.Generic;

namespace ACBrLib.Sat
{
    public sealed class ProdutoSat
    {
        #region Properties

        public string cProd { get; set; }

        public string infAdProd { get; set; }

        public string cEAN { get; set; }

        public string xProd { get; set; }

        public string NCM { get; set; }

        public string CEST { get; set; }

        public string CFOP { get; set; }

        public string uCom { get; set; }

        public bool Combustivel { get; set; }

        public decimal qCom { get; set; }

        public decimal vUnCom { get; set; }

        public decimal vProd { get; set; }

        public IndRegra indRegra { get; set; }

        public decimal vDesc { get; set; }

        public decimal vOutro { get; set; }

        public decimal vItem { get; set; }

        public decimal vRatDesc { get; set; }

        public decimal vRatAcr { get; set; }

        public decimal vItem12741 { get; set; }

        public List<ObsFiscoDetSat> ObsFisco { get; } = new List<ObsFiscoDetSat>();

        public ICMSSat ICMS { get; } = new ICMSSat();

        public PISSat PIS { get; } = new PISSat();

        public PISSTSat PISST { get; } = new PISSTSat();

        public COFINSSat COFINS { get; } = new COFINSSat();

        public COFINSSTSat COFINSST { get; } = new COFINSSTSat();

        public ISSQNSat ISSQN { get; } = new ISSQNSat();

        #endregion Properties
    }
}