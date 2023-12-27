using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class ACBrPIXCDConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public ACBrPIXCDConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.PIXCD)
        {
            Bradesco = new BradescoConfig(acbrlib);
            Sicredi = new SicrediConfig(acbrlib);
            Sicoob = new SicoobConfig(acbrlib);
            Shipay = new ShipayConfig(acbrlib);
            Santander = new SantanderConfig(acbrlib);
            PixPDV = new PixPDVConfig(acbrlib);
            PagSeguro = new PagSeguroConfig(acbrlib);
            Itau = new ItauConfig(acbrlib);
            Inter = new InterConfig(acbrlib);
            GerenciaNet = new GerenciaNetConfig(acbrlib);
            BancoBrasil = new BancoBrasilConfig(acbrlib);
            Ailos = new AilosConfig(acbrlib);
            Matera = new MateraConfig(acbrlib);
            Cielo = new CieloConfig(acbrlib);
            MercadoPago = new MercadoPagoConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public BradescoConfig Bradesco { get; }

        public SicrediConfig Sicredi { get; }

        public SicoobConfig Sicoob { get; }

        public ShipayConfig Shipay { get; }

        public SantanderConfig Santander { get; }

        public PixPDVConfig PixPDV { get; }

        public PagSeguroConfig PagSeguro { get; }

        public ItauConfig Itau { get; }

        public InterConfig Inter { get; }

        public GerenciaNetConfig GerenciaNet { get; }

        public BancoBrasilConfig BancoBrasil { get; }

        public AilosConfig Ailos { get; }

        public MateraConfig Matera { get; }

        public CieloConfig Cielo { get; }

        public MercadoPagoConfig MercadoPago { get; }

        public Ambiente Ambiente
        {
            get => GetProperty<Ambiente>();
            set => SetProperty(value);
        }

        public NivelLogPSP NivelLog
        {
            get => GetProperty<NivelLogPSP>();
            set => SetProperty(value);
        }

        public PSP PSP
        {
            get => GetProperty<PSP>();
            set => SetProperty(value);
        }

        public TipoChave TipoChave
        {
            get => GetProperty<TipoChave>();
            set => SetProperty(value);
        }

        public string ArqLog
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int Timeout
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public string CNPJSoftwareHouse
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string NomeAplicacao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string NomeSoftwareHouse
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string VersaoAplicacao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ProxyHost
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ProxyPass
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int ProxyPort
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public string ProxyUser
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int ChaveCategoriaComerciante
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public string CEPRecebedor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string CidadeRecebedor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string NomeRecebedor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string UFRecebedor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}