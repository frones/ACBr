using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.Core.CTe
{
    public class ConsultaCTeResposta : DFeRespostaBase
    {   
        public VersaoCTe Versao { get; set; }

        public TipoAmbiente TpAmb { get; set; }

        public string VerAplic { get; set; }

        public int CStat { get; set; }

        public string XMotivo { get; set; }

        public int CUF { get; set; }

        public string ChCTe { get; set; }

        public DateTime DhRecbto { get; set; }

        public string NProt { get; set; }

        public string DigVal { get; set; }

        public static ConsultaCTeResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<ConsultaCTeResposta>("Consulta");
            ret.Resposta = resposta;

            return ret;
        }

    }
}
