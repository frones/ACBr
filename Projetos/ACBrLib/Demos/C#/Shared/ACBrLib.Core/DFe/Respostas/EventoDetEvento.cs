using System;
using System.Collections.Generic;

namespace ACBrLib.Core.DFe.Respostas
{
    public sealed class EventoDetEvento
    {
        #region Properties

        public string descEvento { get; set; }
        public string nProtEvento { get; set; }
        public string verAplic { get; set; }
        public List<EventoDetPag> DetPag { get; } = new List<EventoDetPag>();

        #endregion Properties
    }
}
