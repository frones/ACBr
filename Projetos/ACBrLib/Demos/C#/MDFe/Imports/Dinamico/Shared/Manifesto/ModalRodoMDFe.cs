using System.Collections.Generic;

namespace ACBrLib.MDFe
{
    public sealed class ModalRodoMDFe : IModalMDFe
    {
        public TpModalMDFe Tipo => TpModalMDFe.Rodo;

        public string codAgPorto { get; set; }

        public InfANTTMDFe InfANTT { get; } = new InfANTTMDFe();

        public VeicTracaoMDFe VeicTracao { get; } = new VeicTracaoMDFe();

        public List<ReboqueMDFe> Reboque { get; } = new List<ReboqueMDFe>(99);

        public List<LacreMDFe> Lacres { get; } = new List<LacreMDFe>(999);
    }
}