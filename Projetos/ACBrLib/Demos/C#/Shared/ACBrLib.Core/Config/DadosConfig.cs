namespace ACBrLib.Core.Config
{
    public sealed class DadosConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public DadosConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "Emitente.Dados";
        }

        #endregion Constructors

        #region Properties

        public string NomeFantasia
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string InscricaoEstadual
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Telefone
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string CEP
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Endereco
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Numero
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Complemento
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Bairro
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Municipio
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string UF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string CodigoMunicipio
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Email
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}