namespace ACBrLib.Core.Config
{
    public sealed class ProxyConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public ProxyConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.Proxy)
        {
        }

        #endregion Constructors

        #region Properties

        public string Servidor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Porta
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

        #endregion Properties
    }
}