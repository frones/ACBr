using System;
using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class ConsultaNFeRetEventoResposta
    {
        #region Properties

        public string CNPJDest { get; set; }

        public string CStat { get; set; }

        public string Id { get; set; }

        public string NomeArquivo { get; set; }

        public string VerAplic { get; set; }

        public string Versao { get; set; }

        public string XML { get; set; }

        public string XMotivo { get; set; }

        public string cOrgao { get; set; }

        public string cOrgaoAutor { get; set; }

        public string chNFe { get; set; }

        public string dhRegEvento { get; set; }

        public string emailDest { get; set; }

        public string nProt { get; set; }

        public string nSeqEvento { get; set; }

        public string tpAmb { get; set; }

        public string tpEvento { get; set; }

        public string xEvento { get; set; }

        public List<ConsultaNFeItemPedidoResposta> Items { get; } = new List<ConsultaNFeItemPedidoResposta>();

        #endregion Properties

        public static ConsultaNFeRetEventoResposta LerRetorno(ACBrIniFile iniresposta, int id)
        {
            var ret = iniresposta.ReadFromIni<ConsultaNFeRetEventoResposta>($"RetEvento{id:000}");

            var i = 0;
            ConsultaNFeItemPedidoResposta item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<ConsultaNFeItemPedidoResposta>($"ItemPedidoRetEvento{id:000}{i:000}");
                if (item == null) continue;

                ret.Items.Add(item);
            } while (item != null);

            return ret;
        }
    }
}