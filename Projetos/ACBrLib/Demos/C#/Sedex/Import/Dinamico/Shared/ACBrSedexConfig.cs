using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.IBGE;

namespace ACBrLib.IBGE
{
    public sealed class ACBrSedexConfig : ACBrLibConfig<ACBrSedex>
    {
        #region Constructors

        public ACBrSedexConfig(ACBrSedex acbrlib) : base(acbrlib, ACBrSessao.IBGE)
        {
        }

        #endregion Constructors

        #region Properties

        public bool IgnorarCaixaEAcentos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}