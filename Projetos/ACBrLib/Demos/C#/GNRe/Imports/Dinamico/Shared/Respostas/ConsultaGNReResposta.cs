using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.GNRe
{
    public class ConsultaGNReResposta : DFeRespostaBase
    {
        #region Properties

        public TipoAmbiente Ambiente { get; set; }

        public string Codigo { get; set; }

        public string Descricao { get; set; }

        public DFeUF UF { get; set; }

        public string ExigeUfFavorecida { get; set; }

        public string ExigeReceita { get; set; }

        #endregion Properties

        #region Methods
        
        public static ConsultaGNReResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<ConsultaGNReResposta>("Consulta");
            ret.Resposta = resposta;

            return ret;
        }

        #endregion
    }
}