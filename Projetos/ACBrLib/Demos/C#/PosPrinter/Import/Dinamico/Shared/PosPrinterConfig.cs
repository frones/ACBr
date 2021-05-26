using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.PosPrinter;

namespace ACBrLib.PosPrinter
{
    public sealed class PosPrinterConfig : ACBrLibConfig<ACBrPosPrinter>
    {
        #region Constructors

        public PosPrinterConfig(ACBrPosPrinter acbrlib) : base(acbrlib, ACBrSessao.PosPrinter)
        {
            BarrasConfig = new PosPrinterBarrasConfig<ACBrPosPrinter>(acbrlib);
            QrCodeConfig = new PosPrinterQRCodeConfig<ACBrPosPrinter>(acbrlib);
            LogoConfig = new PosPrinterLogoConfig<ACBrPosPrinter>(acbrlib);
            GavetaConfig = new PosPrinterGavetaConfig<ACBrPosPrinter>(acbrlib);
            MPaginaConfig = new PosPrinterMPaginaConfig<ACBrPosPrinter>(acbrlib);
            Device = new DeviceConfig<ACBrPosPrinter>(acbrlib, ACBrSessao.PosPrinter_Device);
        }

        #endregion Constructors

        #region Properties

        public PosPrinterBarrasConfig<ACBrPosPrinter> BarrasConfig { get; }

        public PosPrinterQRCodeConfig<ACBrPosPrinter> QrCodeConfig { get; }

        public PosPrinterLogoConfig<ACBrPosPrinter> LogoConfig { get; }

        public PosPrinterGavetaConfig<ACBrPosPrinter> GavetaConfig { get; }

        public PosPrinterMPaginaConfig<ACBrPosPrinter> MPaginaConfig { get; }

        public DeviceConfig<ACBrPosPrinter> Device { get; }

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