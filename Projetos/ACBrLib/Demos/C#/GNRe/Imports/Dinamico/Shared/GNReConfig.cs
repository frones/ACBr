using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.GNRe;

namespace ACBrLib.GNRe
{
    public sealed class GNReConfig : ACBrLibDFeConfig<ACBrGNRe>
    {
        #region Constructors

        public GNReConfig(ACBrGNRe acbrlib) : base(acbrlib, ACBrSessao.GNRe)
        {
            Guia = new GuiaConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public GuiaConfig Guia { get; }

        public bool EmissaoPathGNRe
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PathGNRe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathArqTxt
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool SalvarTXT
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public VersaoGNRe VersaoDF
        {
            get => GetProperty<VersaoGNRe>();
            set => SetProperty(value);
        }

        public bool SalvarApenasGNReProcessados
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}