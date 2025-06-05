using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe.Respostas;

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

        public EventoDetEvento DetEvento;

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

            ret.DetEvento = iniresposta.ReadFromIni<EventoDetEvento>($"DetEvento");

            if (ret.DetEvento != null)
            {
                i = 0;
                EventoDetPag detPag;
                do
                {
                    i++;
                    detPag = iniresposta.ReadFromIni<EventoDetPag>($"DetPag{i:000}");
                    if (detPag == null) continue;

                    ret.DetEvento.DetPag.Add(detPag);
                } while (detPag != null);
            }

            return ret;
        }

        #endregion Methods
    }
}