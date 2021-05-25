using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public abstract class LibNFeResposta
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

        public string Resposta { get; protected set; }

        #endregion Properties
    }
}