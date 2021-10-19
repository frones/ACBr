using System;
using System.Collections.Generic;

namespace ACBrLib.Core.DFe
{
    public sealed class DistribuicaoDFeResposta<TEvento> where TEvento : Enum
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

        public List<ResEveResposta<TEvento>> ResEveResposta { get; } = new List<ResEveResposta<TEvento>>();

        public List<ProcEveResposta<TEvento>> ProEveResposta { get; } = new List<ProcEveResposta<TEvento>>();

        public List<InfEventoResposta<TEvento>> InfEventoResposta { get; } = new List<InfEventoResposta<TEvento>>();

        #endregion Properties

        #region Methods

        public static DistribuicaoDFeResposta<TEvento> LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<DistribuicaoDFeResposta<TEvento>>("DistribuicaoDFe");
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
            ResEveResposta<TEvento> resEve;
            do
            {
                i++;
                resEve = iniresposta.ReadFromIni<ResEveResposta<TEvento>>($"ResEve{i:000}");
                if (resEve == null) continue;

                ret.ResEveResposta.Add(resEve);
            } while (resEve != null);

            i = 0;
            ProcEveResposta<TEvento> proEve;
            do
            {
                i++;
                proEve = iniresposta.ReadFromIni<ProcEveResposta<TEvento>>($"ProEve{i:000}");
                if (proEve == null) continue;

                ret.ProEveResposta.Add(proEve);
            } while (proEve != null);

            i = 0;
            InfEventoResposta<TEvento> infEvento;
            do
            {
                i++;
                infEvento = iniresposta.ReadFromIni<InfEventoResposta<TEvento>>($"InfEve{i:000}");
                if (infEvento == null) continue;

                ret.InfEventoResposta.Add(infEvento);
            } while (infEvento != null);

            return ret;
        }

        #endregion Methods
    }
}