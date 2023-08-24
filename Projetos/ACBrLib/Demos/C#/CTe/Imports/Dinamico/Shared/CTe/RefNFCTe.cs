using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.CTe;

namespace ACBrLib.Core.CTe
{
    public class RefNFCTe
    {
        public string CNPJCPF { get; set; }

        public string mod { get; set; }

        public int serie { get; set; }

        public int subserie { get; set; }

        public int nro { get; set; }

        public decimal valor { get; set; }

        public DateTime dEmi { get; set; }
    }
}
