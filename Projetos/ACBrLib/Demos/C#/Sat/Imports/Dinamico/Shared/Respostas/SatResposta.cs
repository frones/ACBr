using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public abstract class SatResposta<TResposta> : SatRespostaBase where TResposta : SatResposta<TResposta>, new()
    {
        #region Properties

        public int NumeroSessao { get; set; }

        public int CodigoDeRetorno { get; set; }

        public int CodigoDeErro { get; set; }

        public string MensagemRetorno { get; set; }

        public int CodigoSEFAZ { get; set; }

        public string MensagemSEFAZ { get; set; }

        public string Retorno { get; set; }

        public string Resposta { get; protected set; }

        #endregion Properties

        #region Methods

        protected virtual void LerResposta(ACBrIniFile iniresposta)
        {
        }

        public static TResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<TResposta>("RespostaSat");
            ret.Resposta = resposta;
            ret.LerResposta(iniresposta);

            return ret;
        }

        #endregion Methods
    }
}