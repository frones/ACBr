using System;
using System.Collections.Generic;
using System.Linq;

namespace ACBrLib.Core.DFe
{
    public sealed class RetornoResposta
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

        public string nRec { get; set; }

        public int cMsg { get; set; }

        public string xMsg { get; set; }

        public string Protocolo { get; set; }

        public string ChaveDFe { get; set; }

        public List<RetornoItemResposta> Items { get; } = new List<RetornoItemResposta>();

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static RetornoResposta LerResposta(string resposta, string prefix)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<RetornoResposta>("Retorno");
            ret.Resposta = resposta;

            var sections = iniresposta.Where(x => x.Name.StartsWith(prefix));
            foreach (var section in sections)
            {
                var item = new RetornoItemResposta();
                section.WriteToIni(item);
                ret.Items.Add(item);
            }

            return ret;
        }

        #endregion Methods
    }
}