using ACBrLib.Core.DFe;

namespace ACBrLib.Sat
{
    public sealed class ISSQNSat
    {
        public decimal vDeducISSQN { get; set; }

        public decimal vBC { get; set; }

        public decimal vAliq { get; set; }

        public decimal vISSQN { get; set; }

        public int cMunFG { get; set; }

        public string cListServ { get; set; }

        public string cServTribMun { get; set; }

        public int cNatOp { get; set; }

        public IndIncentivo indIncFisc { get; set; }
    }
}