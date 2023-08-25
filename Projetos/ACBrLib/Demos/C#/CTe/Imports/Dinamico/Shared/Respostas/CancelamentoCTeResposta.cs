using ACBrLib.Core.DFe;
using System;

namespace ACBrLib.Core.CTe
{
    public class CancelamentoCTeResposta
    {
        public string CNPJDest { get; set; }

        public int CStat { get; set; }

        public int CUF { get; set; }

        public DateTime DhRecbto { get; set; }

        public string Msg { get; set; }

        public string VerAplic { get; set; }

        public string Versao { get; set; }

        public string XML { get; set; }

        public string XMotivo { get; set; }

        public string chCTe { get; set; }

        public string emailDest { get; set; }

        public string nProt { get; set; }

        public string nSeqEvento { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public string tpEvento { get; set; }

        public string xEvento { get; set; }

        public string Resposta { get; protected set; }

        public static CancelamentoCTeResposta LerResposta(string resposta)
        {
            ACBrIniFile iniData = ACBrIniFile.Parse(resposta);
            CancelamentoCTeResposta cancelamentoCTeResposta = iniData.ReadFromIni<CancelamentoCTeResposta>("Cancelamento");
            cancelamentoCTeResposta.Resposta = resposta;
            return cancelamentoCTeResposta;
        }
    }
}
