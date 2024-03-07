using ACBrLib.Core;
using ACBrLib.Core.Config;
using static ACBrLib.ConsultaCNPJ.ACBrConsultaCNPJ;

namespace ACBrLib.ConsultaCNPJ
{
    public sealed class ACBrCNPJConfig : ACBrLibConfig<ACBrConsultaCNPJ>
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

        #region Metodos

        #endregion Metodos

    }





}