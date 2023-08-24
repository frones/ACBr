using ACBrLib.Core.CTe;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.CTe
{
    public class VeiculosCTe
    {
        public string placa { get; set; }

        public string RENAVAM { get; set; }

        public Proprietario prop { get; set; } = new Proprietario();

        public string UF { get; set; }
    }
}
