using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class PixPDVConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public PixPDVConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.PixPDV)
        {

        }

        #endregion Constructors

        #region Properties

        public string CNPJ
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Token
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string SecretKey
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }



        #endregion Properties
    }
}