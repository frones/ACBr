using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.CTe;

namespace ACBrLib.Core.CTe
{
    public class InfCteSubCTe
    {   
        public long chCte { get; set; }

        public long refCteAnu { get; set; }

        public TomaICMSCTe tomaICMS { get; set; } = new TomaICMSCTe();

        public int indAlteraToma { get; set; }
    }
}
