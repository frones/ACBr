namespace ACBrLib.Core.Config
{
    public sealed class EmissorConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public EmissorConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.Emissor)
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