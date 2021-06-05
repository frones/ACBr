using System;
using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public sealed class StatusServicoResposta : LibNFeResposta
    {
        #region Properties

        public int TMed { get; set; }

        public DateTime DhRetorno { get; set; }

        public string XObs { get; set; }

        #endregion Properties

        #region Methods

        public static StatusServicoResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<StatusServicoResposta>("Status");
            ret.Resposta = resposta;
            return ret;
        }

        #endregion Methods
    }
}