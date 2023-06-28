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
        public string chCte { get; set; }

        public string refCteAnu { get; set; }

        public TomaICMSCTe tomaICMS { get; set; } = new TomaICMSCTe();

        public int indAlteraToma { get; set; }
    }
}
