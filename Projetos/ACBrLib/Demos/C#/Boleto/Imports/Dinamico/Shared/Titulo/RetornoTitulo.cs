using System;
using System.Collections.Generic;

namespace ACBrLib.Boleto
{
    public sealed class RetornoTitulo
    {
        #region Properties

        public RetornoSacado Sacado { get; } = new RetornoSacado();

        public DateTime Vencimento { get; set; }

        public DateTime DataDocumento { get; set; }

        public string NumeroDocumento { get; set; }

        public DateTime DataProcessamento { get; set; }

        public string NossoNumero { get; set; }

        public string Carteira { get; set; }

        public decimal ValorDocumento { get; set; }

        public DateTime DataOcorrencia { get; set; }

        public DateTime DataCredito { get; set; }

        public DateTime? DataBaixa { get; set; }

        public DateTime? DataMoraJuros { get; set; }

        public decimal ValorDespesaCobranca { get; set; }

        public decimal ValorAbatimento { get; set; }

        public decimal ValorDesconto { get; set; }

        public decimal ValorMoraJuros { get; set; }

        public decimal ValorIOF { get; set; }

        public decimal ValorOutrasDespesas { get; set; }

        public decimal ValorOutrosCreditos { get; set; }

        public decimal ValorRecebido { get; set; }

        public decimal ValorPago { get; set; }

        public string SeuNumero { get; set; }
        public string EMV { get; set; }

        public string CodTipoOcorrencia { get; set; }

        public string DescricaoTipoOcorrencia { get; set; }

        public int LiquidadoBanco { get; set; }

        public List<RetornoRejeicao> Rejeicoes { get; } = new List<RetornoRejeicao>();

        #endregion Properties
    }
}