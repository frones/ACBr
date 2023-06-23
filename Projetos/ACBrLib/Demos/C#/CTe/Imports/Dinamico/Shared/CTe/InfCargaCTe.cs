using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class InfCargaCTe
    {
        public decimal vCarga { get; set; }

        public string proPred { get; set; }

        public string xOutCat { get; set; }

        public InfQCTe infQ { get; set; } = new InfQCTe();

        public decimal vCargaAverb { get; set; }
    }
}
