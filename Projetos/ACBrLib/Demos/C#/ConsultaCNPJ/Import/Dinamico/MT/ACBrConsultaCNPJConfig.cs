using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;

namespace ACBrLib.ConsultaCNPJ
{
    public sealed class ACBrCNPJConfig : ACBrLibDFeConfig<ACBrConsultaCNPJ>
    {
        #region Constructors

        public ACBrCNPJConfig(ACBrConsultaCNPJ acbrlib) : base(acbrlib, ACBrSessao.ConsultaCNPJ)
        {

        }



        #endregion Constructors

        #region Properties

        public int Provedor
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public string Usuario
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