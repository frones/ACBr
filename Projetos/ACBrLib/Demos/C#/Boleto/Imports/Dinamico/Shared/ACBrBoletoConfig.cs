using ACBrLib.Core;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    /// <summary>
    /// Configurações da Sessão [BoletoBancoConfig]
    /// </summary>
    public sealed class ACBrBoletoConfig : ACBrLibConfig<ACBrBoleto>
    {
        #region Constructors

        /// <summary>
        /// Inicializa uma nova instancia da classe  <see cref="ACBrBoletoConfig"/>.
        /// </summary>
        /// <param name="acbrboleto">Instancia do ACBrBoleto</param>
        public ACBrBoletoConfig(ACBrBoleto acbrboleto) : base(acbrboleto, ACBrSessao.BoletoConfig)
        {
            Banco = new BancoConfig(acbrboleto);
            Cedente = new CedenteConfig(acbrboleto);
            Diretorio = new DiretorioConfig(acbrboleto);
            Impressao = new ImpressaoConfig(acbrboleto);
            Webservice = new WebserviceConfig(acbrboleto);
            CedenteWebservice = new CedenteWebserviceConfig(acbrboleto);
            Email = new EmailConfig<ACBrBoleto>(acbrboleto);
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Configurações da Sessão [BoletoBancoConfig]
        /// </summary>
        public BancoConfig Banco { get; }

        /// <summary>
        /// Configurações da Sessão [BoletoCedenteConfig]
        /// </summary>
        public CedenteConfig Cedente { get; }

        /// <summary>
        /// Configurações da Sessão [BoletoCedenteConfig]
        /// </summary>
        public DiretorioConfig Diretorio { get; }

        /// <summary>
        /// Configurações da Sessão [BoletoBancoFCFortesConfig]
        /// </summary>
        public ImpressaoConfig Impressao { get; }

        /// <summary>
        /// Configurações da Sessão [BoletoWebSevice]
        /// </summary>
        public WebserviceConfig Webservice { get; }

        /// <summary>
        /// Configurações da Sessão [BoletoCedenteWS]
        /// </summary>
        public CedenteWebserviceConfig CedenteWebservice { get; }

        /// <summary>
        /// Configurações da Sessão [Email]
        /// </summar
        public EmailConfig<ACBrBoleto> Email { get; }

        /// <summary>
        /// Assunto padrão do envio de email.
        /// </summary>
        public string EmailAssuntoBoleto
        {
            get => GetProperty<string>("emailAssuntoBoleto");
            set => SetProperty(value, "emailAssuntoBoleto");
        }

        /// <summary>
        /// Mensagem que será enviada no email por padrão.
        /// </summary>
        public string EmailMensagemBoleto
        {
            get => GetProperty<string>("emailMensagemBoleto");
            set => SetProperty(value, "emailMensagemBoleto");
        }

        #endregion Properties
    }
}