using System;
using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class ModalFerroviarioMDFe : IModalMDFe
    {
        public TpModalMDFe Tipo => TpModalMDFe.Ferroviario;

        public string xPref { get; set; }

        public DateTime dhTrem { get; set; }

        public string xOri { get; set; }

        public string xDest { get; set; }

        public int qVag { get; set; }

        public List<VagaoMDFe> Vagao { get; } = new List<VagaoMDFe>();
    }
}