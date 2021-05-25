using System;

namespace ACBrLib.NFe
{
    public class MedicamentoNFe
    {
        public string cProdANVISA { get; set; }

        public decimal vPMC { get; set; }

        public string nLote { get; set; }

        public decimal qLote { get; set; }

        public DateTime dFab { get; set; }

        public DateTime dVal { get; set; }

        public string xMotivoIsencao { get; set; }
    }
}