using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class QrCode
    {
        #region Properties
        public string emv { get; set; }
        public string txid { get; set; }
        public string url { get; set; }
        #endregion Properties
    }
}
