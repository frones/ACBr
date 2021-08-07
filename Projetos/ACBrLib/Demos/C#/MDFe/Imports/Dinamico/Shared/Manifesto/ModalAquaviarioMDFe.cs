using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class ModalAquaviarioMDFe : IModalMDFe
    {
        public TpModalMDFe Tipo => TpModalMDFe.Aquaviario;

        public string CNPJAgeNav { get; set; }

        public string irin { get; set; }

        public string tpEmb { get; set; }

        public string cEmbar { get; set; }

        public string xEmbar { get; set; }

        public string nViag { get; set; }

        public string cPrtEmb { get; set; }

        public string cPrtDest { get; set; }

        public string prtTrans { get; set; }

        public TipoNavegacao tpNav { get; set; }

        public List<InfTerminalCarregMDFe> InfTermCarreg { get; } = new List<InfTerminalCarregMDFe>();

        public List<InfTerminalDescarregMDFe> InfTermDescarreg { get; } = new List<InfTerminalDescarregMDFe>();

        public List<InfEmbCombMDFe> InfEmbComb { get; } = new List<InfEmbCombMDFe>();

        public List<InfUnidCargaVaziaMDFe> InfUnidCargaVazia { get; } = new List<InfUnidCargaVaziaMDFe>();

        public List<InfUnidTranspVaziaMDFe> InfUnidTranspVazia { get; } = new List<InfUnidTranspVaziaMDFe>();
    }
}