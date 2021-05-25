using ACBrLib.Core;
using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public sealed class CancelamentoResposta : LibNFeResposta
    {
        #region Properties

        public string chNFe { get; set; }

        public string nProt { get; set; }

        public string tpEvento { get; set; }

        public string xEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string CNPJDest { get; set; }

        public string emailDest { get; set; }

        public string XML { get; set; }

        public string Arquivo { get; set; }

        #endregion Properties

        #region Methods

        public static CancelamentoResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<CancelamentoResposta>("Cancelamento");
            ret.Resposta = resposta;
            return ret;
        }

        #endregion Methods
    }
}