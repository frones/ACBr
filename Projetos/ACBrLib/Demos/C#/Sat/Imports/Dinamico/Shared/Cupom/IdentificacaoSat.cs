using System;
using ACBrLib.Core.DFe;

namespace ACBrLib.Sat
{
    public sealed class IdentificacaoSat
    {
        #region Properties

        public int? cUF { get; set; }

        public int? cNF { get; set; }

        public int? mod { get; set; }

        public int? nserieSAT { get; set; }

        public int? nCFe { get; set; }

        public DateTime dhEmi { get; set; }

        public int? cDV { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        public string CNPJ { get; set; }

        public string signAC { get; set; }

        public string assinaturaQRCODE { get; set; }

        public string numeroCaixa { get; set; }

        #endregion Properties
    }
}