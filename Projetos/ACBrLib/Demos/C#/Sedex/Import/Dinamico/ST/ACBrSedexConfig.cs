using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Sedex;

namespace ACBrLib.Sedex
{
    public sealed class ACBrSedexConfig : ACBrLibConfig<ACBrSedex>
    {
        #region Constructors

        public ACBrSedexConfig(ACBrSedex acbrlib) : base(acbrlib, ACBrSessao.Sedex)
        {

        }

        #endregion Constructors

        #region Properties
            

        public string CodContrato
        {
            get => GetProperty<string>();
            set => SetProperty(value);

        }

        public string Senha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }


        #endregion Properties
    }
}