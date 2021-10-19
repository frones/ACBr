using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class EventoEncerramento : EventoMDFeBase
    {
        #region Constructor

        public EventoEncerramento()
        {
            tpEvento = TipoEventoMDFe.teEncerramento;
            versaoEvento = "3.00";
        }

        #endregion Constructor

        #region Properties

        public string nProt { get; set; }

        public DateTime dtEnc { get; set; }

        public int cUF { get; set; }

        public int cMun { get; set; }

        #endregion Properties
    }
}