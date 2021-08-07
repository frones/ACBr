using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class SeguroMDFe
    {
        public RspSegMDFe respSeg { get; set; } = RspSegMDFe.rsEmitente;

        public string CNPJCPF { get; set; }

        public string xSeg { get; set; }

        public string CNPJ { get; set; }

        public string nApol { get; set; }

        public List<AverbacaoMDFe> Averb { get; } = new List<AverbacaoMDFe>();
    }
}