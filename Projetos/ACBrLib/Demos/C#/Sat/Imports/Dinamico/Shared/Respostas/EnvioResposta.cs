using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class EnvioResposta : SatRespostaBase
    {
        #region Properties

        public string Resultado { get; set; }

        public int NumeroSessao { get; set; }

        public int CodigoDeRetorno { get; set; }

        public string RetornoStr { get; set; }

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static EnvioResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<EnvioResposta>("CFE");
            ret.Resposta = resposta;

            return ret;
        }

        #endregion Methods
    }
}