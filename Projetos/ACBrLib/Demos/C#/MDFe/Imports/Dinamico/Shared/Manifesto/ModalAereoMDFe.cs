using System;

namespace ACBrLib.MDFe
{
    public sealed class ModalAereoMDFe : IModalMDFe
    {
        public TpModalMDFe Tipo => TpModalMDFe.Aereo;

        public string nac { get; set; }

        public string matr { get; set; }

        public string nVoo { get; set; }

        public string cAerEmb { get; set; }

        public string cAerDes { get; set; }

        public DateTime dVoo { get; set; }
    }
}