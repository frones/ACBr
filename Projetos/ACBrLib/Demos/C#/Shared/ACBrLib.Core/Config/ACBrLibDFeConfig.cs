using ACBrLib.Core.DFe;

namespace ACBrLib.Core.Config
{
    public abstract class ACBrLibDFeConfig<TLib> : ACBrLibConfig<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        protected ACBrLibDFeConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            DFe = new DFeConfig<TLib>(Parent);
            Email = new EmailConfig<TLib>(Parent);
            Download = new DownloadConfig<TLib>(Parent, sessao);
            Emitente = new EmitenteConfig<TLib>(Parent, sessao);
        }

        #endregion Constructors

        #region Properties

        public DFeConfig<TLib> DFe { get; }

        public EmailConfig<TLib> Email { get; set; }

        public DownloadConfig<TLib> Download { get; }

        public EmitenteConfig<TLib> Emitente { get; }

        public TipoAmbiente Ambiente
        {
            get => GetProperty<TipoAmbiente>();
            set => SetProperty(value);
        }

        public TipoEmissao FormaEmissao
        {
            get => GetProperty<TipoEmissao>();
            set => SetProperty(value);
        }

        public bool SalvarGer
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ExibirErroSchema
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string FormatoAlerta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool RetirarAcentos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool RetirarEspacos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool IdentarXML
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ValidarDigest
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PathSalvar
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathSchemas
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string IniServicos
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool SalvarArq
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool AdicionarLiteral
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorCNPJ
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorModelo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorAno
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorMes
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorDia
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SalvarWS
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int Timeout
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TimeoutPorThread
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool Visualizar
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool AjustaAguardaConsultaRet
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int AguardarConsultaRet
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int IntervaloTentativas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Tentativas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public SSLType SSLType
        {
            get => GetProperty<SSLType>();
            set => SetProperty(value);
        }

        public string QuebradeLinha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}