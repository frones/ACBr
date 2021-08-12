using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.IBGE;

namespace ACBrLib.IBGE
{
    public sealed class ACBrCEPConfig : ACBrLibConfig<ACBrIBGE>
    {
        #region Constructors

		///////////////////////////////////////////////////////////
				///////////////////////////////////////////////////////////
						///////////////////////////////////////////////////////////

        public ACBrCEPConfig(ACBrIBGE acbrlib) : base(acbrlib, ACBrSessao.IBGE)
        {
        }

        #endregion Constructors

        #region Properties

        public IgnorarCaixaEAcentos IgnorarCaixaEAcentos
        {
            get => GetProperty<IgnorarCaixaEAcentos>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}