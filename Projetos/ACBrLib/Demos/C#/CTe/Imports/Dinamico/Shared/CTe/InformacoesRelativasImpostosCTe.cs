using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class InformacoesRelativasImpostosCTe
    {
        public ICMS00CTe ICMS00 { get; } = new ICMS00CTe();

        public ICMS20CTe ICMS20 { get; set; } = new ICMS20CTe();

        public ICMS45CTe ICMS45 { get; set; } = new ICMS45CTe();

        public ICMS60CTe ICMS60 { get; set; } = new ICMS60CTe();

        public ICMS90CTe ICMS90 { get; set; } = new ICMS90CTe();

        public ICMSOutraUFCTe ICMSOutraUF { get; set; } = new ICMSOutraUFCTe();

        public ICMSSNCTe ICMSSN { get; set; } = new ICMSSNCTe();

        public ICMSUFFimCTe ICMSUFFim { get; set; } = new ICMSUFFimCTe();

        public decimal vTotTrib { get; set; }

        public string infAdFisco { get; set; }

        public InfTributacaoFederal infTribFed { get; set; } = new InfTributacaoFederal();

    }
}
