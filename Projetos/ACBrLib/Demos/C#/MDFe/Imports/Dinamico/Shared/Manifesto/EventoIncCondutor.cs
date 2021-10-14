using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class EventoIncCondutor : EventoMDFeBase
    {
        #region Constructor

        public EventoIncCondutor()
        {
            evento = TipoEvento.teInclusaoCondutor;
            versaoEvento = "3.00";
        }

        #endregion Constructor

        #region Properties

        public string xNome { get; set; }

        public string CPF { get; set; }

        #endregion Properties
    }
}