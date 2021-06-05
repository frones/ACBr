using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class RetornoCriarCFeResposta : SatRespostaBase
    {
        #region Properties

        public string nCFe { get; set; }

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static RetornoCriarCFeResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<RetornoCriarCFeResposta>("CFE");
            ret.Resposta = resposta;

            return ret;
        }

        #endregion Methods
    }
}