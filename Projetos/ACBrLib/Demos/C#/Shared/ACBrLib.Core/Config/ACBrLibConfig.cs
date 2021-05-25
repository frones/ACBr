namespace ACBrLib.Core.Config
{
    public abstract class ACBrLibConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        protected ACBrLibConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            Principal = new PrincipalConfig<TLib>(Parent);
            Sistema = new SistemaConfig<TLib>(Parent);
            Proxy = new ProxyConfig<TLib>(Parent);
            SoftwareHouse = new SoftwareHouseConfig<TLib>(Parent);
            Emissor = new EmissorConfig<TLib>(Parent);
        }

        #endregion Constructors

        #region Properties

        public PrincipalConfig<TLib> Principal { get; }

        public SistemaConfig<TLib> Sistema { get; }

        public ProxyConfig<TLib> Proxy { get; }

        public SoftwareHouseConfig<TLib> SoftwareHouse { get; }

        public EmissorConfig<TLib> Emissor { get; set; }

        #endregion Properties
    }
}