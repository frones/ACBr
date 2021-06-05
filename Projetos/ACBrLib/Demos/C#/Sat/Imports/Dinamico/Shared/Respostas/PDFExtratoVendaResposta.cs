using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class PDFExtratoVendaResposta : SatRespostaBase
    {
        #region Properties

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static PDFExtratoVendaResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<PDFExtratoVendaResposta>("CFe");
            ret.Resposta = resposta;

            return ret;
        }

        #endregion Methods
    }
}