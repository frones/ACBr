using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public sealed class EventoResposta : LibNFeResposta
    {
        #region Properties

        public int idLote { get; set; }

        public int cOrgao { get; set; }

        public List<EventoItemResposta> Items { get; } = new List<EventoItemResposta>();

        #endregion Properties

        #region Methods

        public static EventoResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<EventoResposta>("Evento");
            ret.Resposta = resposta;

            var i = 0;
            EventoItemResposta item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<EventoItemResposta>($"Evento{i:000}");
                if (item == null) continue;

                ret.Items.Add(item);
            } while (item != null);

            return ret;
        }

        #endregion Methods
    }
}