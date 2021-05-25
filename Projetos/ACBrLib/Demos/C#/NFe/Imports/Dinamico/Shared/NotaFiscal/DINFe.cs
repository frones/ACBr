using System;
using System.Collections.Generic;

namespace ACBrLib.NFe
{
    public class DINFe
    {
        public string nDi { get; set; }

        public DateTime dDi { get; set; }

        public string xLocDesemb { get; set; }

        public string UFDesemb { get; set; }

        public DateTime dDesemb { get; set; }

        public TipoViaTransp tpViaTransp { get; set; }

        public decimal vAFRMM { get; set; }

        public string tpIntermedio { get; set; }

        public string CNPJ { get; set; }

        public string UFTerceiro { get; set; }

        public string cExportador { get; set; }

        public List<LADINFe> LADI { get; } = new List<LADINFe>();
    }
}