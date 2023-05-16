namespace ACBrLib.Core.Config
{
    public sealed class EmitenteConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public EmitenteConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "Emitente";

            Dados = new DadosConfig<TLib>(Parent, sessao);
        }

        #endregion Constructors

        #region Properties

        public DadosConfig<TLib> Dados { get; set; }

        public string CNPJ
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string InscMun
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string RazSocial
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string WSUser
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string WSSenha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string WSFraseSecr
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string WSChaveAcesso
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string WSChaveAutoriz
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}