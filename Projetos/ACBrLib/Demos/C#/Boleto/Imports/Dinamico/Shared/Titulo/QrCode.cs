using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class QrCode
    {
        #region Properties
        [IniKey("QrCode.emv")]
        public string emv { get; set; }
        [IniKey("QrCode.txid")]
        public string txid { get; set; }
        [IniKey("QrCode.url")]
        public string url { get; set; }
        #endregion Properties
    }
}
