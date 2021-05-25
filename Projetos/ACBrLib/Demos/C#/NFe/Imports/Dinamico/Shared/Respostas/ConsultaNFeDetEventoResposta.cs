using System;
using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.Ini;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class ConsultaNFeDetEventoResposta
    {
        #region Properties

        public string Versao { get; set; }

        public string VerAplic { get; set; }

        public string descEvento { get; set; }

        public string xCorrecao { get; set; }

        public string xCondUso { get; set; }

        public string nProt { get; set; }

        public string xJust { get; set; }

        public string cOrgaoAutor { get; set; }

        public string tpAutor { get; set; }

        public DateTime dhEmi { get; set; }

        public TipoNFe tpNF { get; set; }

        public string IE { get; set; }

        public string DESTCNPJCPF { get; set; }

        public string DESTidEstrangeiro { get; set; }

        public string DESTIE { get; set; }

        public string DESTUF { get; set; }

        public decimal vNF { get; set; }

        public decimal vICMS { get; set; }

        public decimal vST { get; set; }

        public string idPedidoCancelado { get; set; }

        public List<ConsultaNFeItemPedidoResposta> Items { get; } = new List<ConsultaNFeItemPedidoResposta>();

        #endregion Properties

        public static ConsultaNFeDetEventoResposta LerRetorno(ACBrIniFile iniresposta, int id)
        {
            var ret = iniresposta.ReadFromIni<ConsultaNFeDetEventoResposta>($"DetEvento{id:000}");

            var i = 0;
            ConsultaNFeItemPedidoResposta item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<ConsultaNFeItemPedidoResposta>($"ItemPedido{id:000}{i:000}");
                if (item == null) continue;

                ret.Items.Add(item);
            } while (item != null);

            return ret;
        }
    }
}