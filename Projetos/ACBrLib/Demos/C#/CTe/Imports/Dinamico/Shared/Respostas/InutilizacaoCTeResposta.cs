using ACBrLib.Core.DFe;
using System;

namespace ACBrLib.Core.CTe
{
    public class InutilizacaoCTeResposta
    {
        public int CStat { get; set; }

        public int CUF { get; set; }

        public DateTime DhRecbto { get; set; }

        public string Msg { get; set; }

        public string NomeArquivo { get; set; }

        public string VerAplic { get; set; }

        public string Versao { get; set; }

        public string XMotivo { get; set; }

        public string Xml { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public string Resposta { get; protected set; }

        public static InutilizacaoCTeResposta LerResposta(string resposta)
        {
            ACBrIniFile iniData = ACBrIniFile.Parse(resposta);
            InutilizacaoCTeResposta inutilizacaoCTeResposta = iniData.ReadFromIni<InutilizacaoCTeResposta>("Inutilizacao");
            inutilizacaoCTeResposta.Resposta = resposta;
            return inutilizacaoCTeResposta;
        }
    }
}
