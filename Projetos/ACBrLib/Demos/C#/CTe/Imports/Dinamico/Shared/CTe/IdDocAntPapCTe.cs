using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class IdDocAntPapCTe
    {
        public TipoDocAnteriorCTe tpDoc { get; set; }

        public string serie { get; set; }
 
        public string subser { get; set; }

        public string nDoc { get; set; }

        public DateTime dEmi { get; set; }
    }
}
