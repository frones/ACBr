using ACBrLib.Core.GNRe;
using ACBrLib.Core.DFe;
using System;

namespace ACBrLib.GNRe
{
    public class ReferenciaGNRe
    {
        #region Properties

        public string Convenio { get; set; }

        public string Receita { get; set; }

        public DFeUF UfFavorecida { get; set; }

        public DateTime DataVencimento { get; set; }

        public DateTime DataPagamento { get; set; }

        public string ReferenciaAno { get; set; }

        public string ReferenciaMes { get; set; }

        public string ReferenciaParcela { get; set; }

        public string ReferenciaPeriodo { get; set; }

        public decimal ValorTotal { get; set; }

        public decimal ValorPrincipal { get; set; }

        #endregion Properties
    }
}