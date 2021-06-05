using System;
using System.Collections.Generic;

namespace ACBrLib.Core.DFe
{
    public sealed class ConsultaCadastroResposta
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

        public string IE { get; set; }

        public string CNPJ { get; set; }

        public string CPF { get; set; }

        public string UF { get; set; }

        public DateTime dhCons { get; set; }

        public List<ConsultaCadastroItemResposta> Items { get; } = new List<ConsultaCadastroItemResposta>();

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static ConsultaCadastroResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<ConsultaCadastroResposta>("ConsultaCadastro");
            ret.Resposta = resposta;

            var i = 0;
            ConsultaCadastroItemResposta item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<ConsultaCadastroItemResposta>($"INFCAD{i:000}");
                if (item == null) continue;

                ret.Items.Add(item);
            } while (item != null);

            return ret;
        }

        #endregion Methods
    }
}