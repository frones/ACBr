using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.CEP;

namespace ACBrLib.CEP
{
    public sealed class ACBrCEPConfig : ACBrLibConfig<ACBrCEP>
    {
        #region Constructors

        public ACBrCEPConfig(ACBrCEP acbrlib) : base(acbrlib, ACBrSessao.CEP)
        {
        }

        #endregion Constructors

        #region Properties

        public WebService WebService
        {
            get => GetProperty<WebService>();
            set => SetProperty(value);
        }

        public string ChaveAcesso
        {
            get => GetProperty<string>();
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

        public bool PesquisarIBGE
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}