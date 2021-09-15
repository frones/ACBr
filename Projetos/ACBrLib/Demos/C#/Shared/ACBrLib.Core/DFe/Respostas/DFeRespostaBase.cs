using System;

namespace ACBrLib.Core.DFe
{
    public abstract class DFeRespostaBase
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

        #region Methods

        public override string ToString() => Resposta;

        public static implicit operator string(DFeRespostaBase source) => source.Resposta;

        #endregion Methods
    }
}