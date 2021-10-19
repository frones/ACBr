using System;
using System.Collections.Generic;

namespace ACBrLib.Core.DFe
{
    public abstract class EventoResposta<TClass, TEvento, TTipoEvento> : DFeRespostaBase
        where TEvento : EventoItemBase<TTipoEvento>, new()
        where TClass : EventoResposta<TClass, TEvento, TTipoEvento>, new()
        where TTipoEvento : Enum

    {
        #region Properties

        public int idLote { get; set; }

        public int cOrgao { get; set; }

        public List<TEvento> Items { get; } = new List<TEvento>();

        #endregion Properties

        #region Methods

        public static TClass LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<TClass>("Evento");
            ret.Resposta = resposta;

            var i = 0;
            TEvento item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<TEvento>($"Evento{i:000}");
                if (item == null) continue;

                ret.Items.Add(item);
            } while (item != null);

            return ret;
        }

        #endregion Methods
    }
}