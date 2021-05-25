using ACBrLib.Core.PosPrinter;

namespace ACBrLib.Core.Config
{
    public sealed class PosPrinterConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PosPrinterConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.PosPrinter)
        {
            BarrasConfig = new PosPrinterBarrasConfig<TLib>(acbrlib);
            QrCodeConfig = new PosPrinterQRCodeConfig<TLib>(acbrlib);
            LogoConfig = new PosPrinterLogoConfig<TLib>(acbrlib);
            GavetaConfig = new PosPrinterGavetaConfig<TLib>(acbrlib);
            MPaginaConfig = new PosPrinterMPaginaConfig<TLib>(acbrlib);
            DeviceConfig = new DeviceConfig<TLib>(acbrlib, ACBrSessao.PosPrinter_Device);
        }

        #endregion Constructors

        #region Properties

        public PosPrinterBarrasConfig<TLib> BarrasConfig { get; }

        public PosPrinterQRCodeConfig<TLib> QrCodeConfig { get; }

        public PosPrinterLogoConfig<TLib> LogoConfig { get; }

        public PosPrinterGavetaConfig<TLib> GavetaConfig { get; }

        public PosPrinterMPaginaConfig<TLib> MPaginaConfig { get; }

        public DeviceConfig<TLib> DeviceConfig { get; }

        public string ArqLog
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public ACBrPosPrinterModelo Modelo
        {
            get => GetProperty<ACBrPosPrinterModelo>();
            set => SetProperty(value);
        }

        public string Porta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public PosPaginaCodigo PaginaDeCodigo
        {
            get => GetProperty<PosPaginaCodigo>();
            set => SetProperty(value);
        }

        public int ColunasFonteNormal
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int EspacoEntreLinhas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int LinhasEntreCupons
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool CortaPapel
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool TraduzirTags
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool IgnorarTags
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int LinhasBuffer
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool ControlePorta
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool VerificarImpressora
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool TipoCorte
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}