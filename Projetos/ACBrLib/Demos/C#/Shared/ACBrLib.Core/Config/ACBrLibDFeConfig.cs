namespace ACBrLib.Core.Config
{
    public abstract class ACBrLibDFeConfig<TLib> : ACBrLibConfig<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        protected ACBrLibDFeConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            DFe = new DFeConfig<TLib>(Parent);
            Email = new EmailConfig<TLib>(Parent);
        }

        #endregion Constructors

        #region Properties

        public DFeConfig<TLib> DFe { get; }

        public EmailConfig<TLib> Email { get; set; }

        #endregion Properties
    }
}