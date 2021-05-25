using System;
using System.Collections.Generic;
using ACBrLib.Core.Ini;

namespace ACBrLib.Core.DFe
{
    public sealed class DistribuicaoDFeResposta
    {
        #region Properties

        public string Msg { get; set; }

        public string Versao { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public string VerAplic { get; set; }

        public int CStat { get; set; }

        public string XMotivo { get; set; }

        public int CUF { get; set; }

        public DateTime DhRecbto { get; set; }

        public DateTime dhResp { get; set; }

        public string ultNSU { get; set; }

        public string maxNSU { get; set; }

        public string arquivo { get; set; }

        public string indCont { get; set; }

        public string Resposta { get; private set; }

        public List<ResDFeResposta> ResDFeResposta { get; } = new List<ResDFeResposta>();

        public List<ResEveResposta> ResEveResposta { get; } = new List<ResEveResposta>();

        public List<ProcEveResposta> ProEveResposta { get; } = new List<ProcEveResposta>();

        public List<InfEventoResposta> InfEventoResposta { get; } = new List<InfEventoResposta>();

        #endregion Properties

        #region Methods

        public static DistribuicaoDFeResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<DistribuicaoDFeResposta>("DistribuicaoDFe");
            ret.Resposta = resposta;

            var i = 0;
            ResDFeResposta resDFe;
            do
            {
                i++;
                resDFe = iniresposta.ReadFromIni<ResDFeResposta>($"ResDFe{i:000}");
                if (resDFe == null) continue;

                ret.ResDFeResposta.Add(resDFe);
            } while (resDFe != null);

            i = 0;
            ResEveResposta resEve;
            do
            {
                i++;
                resEve = iniresposta.ReadFromIni<ResEveResposta>($"ResEve{i:000}");
                if (resEve == null) continue;

                ret.ResEveResposta.Add(resEve);
            } while (resEve != null);

            i = 0;
            ProcEveResposta proEve;
            do
            {
                i++;
                proEve = iniresposta.ReadFromIni<ProcEveResposta>($"ProEve{i:000}");
                if (proEve == null) continue;

                ret.ProEveResposta.Add(proEve);
            } while (proEve != null);

            i = 0;
            InfEventoResposta infEvento;
            do
            {
                i++;
                infEvento = iniresposta.ReadFromIni<InfEventoResposta>($"InfEve{i:000}");
                if (infEvento == null) continue;

                ret.InfEventoResposta.Add(infEvento);
            } while (infEvento != null);

            return ret;
        }

        #endregion Methods
    }
}