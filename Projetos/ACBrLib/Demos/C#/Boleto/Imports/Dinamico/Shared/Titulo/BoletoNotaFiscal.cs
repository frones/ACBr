using System;

namespace ACBrLib.Boleto
{
    public sealed class BoletoNotaFiscal
    {
        #region Properties

        public string NumNFe { get; set; }

        public string ChaveNFe { get; set; }

        public decimal ValorNFe { get; set; }

        public DateTime EmissaoNFe { get; set; }

        #endregion Properties
    }
}