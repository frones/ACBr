using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public class FluxoCTe
    {
        public string xOrig { get; set; }

        public List<PassCTe> Pass { get; } = new List<PassCTe>();

        public string xDest { get; set; }

        public int xRota { get; set; }
    }
}
