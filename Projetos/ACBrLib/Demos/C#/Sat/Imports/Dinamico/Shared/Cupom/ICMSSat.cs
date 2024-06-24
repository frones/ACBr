using ACBrLib.Core.DFe;

namespace ACBrLib.Sat
{
    public sealed class ICMSSat
    {
        #region Properties

        public OrigemMercadoria orig { get; set; }

        public CSTIcms CST { get; set; }

        public CSOSNIcms CSOSN { get; set; }

        public decimal pICMS { get; set; }

        public decimal vICMS { get; set; }

        #endregion Properties
    }
}