namespace ACBrLib.Core.Config
{
    public sealed class SoftwareHouseConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public SoftwareHouseConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.SoftwareHouse)
        {
        }

        #endregion Constructors

        #region Properties

        public string CNPJ
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string RazaoSocial
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string NomeFantasia
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string WebSite
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Email
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Telefone
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Responsavel
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}