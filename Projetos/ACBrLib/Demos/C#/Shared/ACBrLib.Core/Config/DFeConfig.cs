using System.IO;
using ACBrLib.Core.DFe;

namespace ACBrLib.Core.Config
{
    public sealed class DFeConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public DFeConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.DFe)
        {
        }

        #endregion Constructors

        #region Properties

        public string ArquivoPFX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string NumeroSerie
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string DadosPFX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Senha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool VerificarValidade
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public SSLCryptLib SSLCryptLib
        {
            get => GetProperty<SSLCryptLib>();
            set => SetProperty(value);
        }

        public SSLHttpLib SSLHttpLib
        {
            get => GetProperty<SSLHttpLib>();
            set => SetProperty(value);
        }

        public SSLXmlSignLib SSLXmlSignLib
        {
            get => GetProperty<SSLXmlSignLib>();
            set => SetProperty(value);
        }

        public string UF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public TimezoneMode TimeZoneModo
        {
            get => GetProperty<TimezoneMode>("TimeZone.Modo");
            set => SetProperty(value, "TimeZone.Modo");
        }

        public string TimeZoneStr
        {
            get => GetProperty<string>("TimeZone.Str");
            set => SetProperty(value, "TimeZone.Str");
        }

        #endregion Properties
    }
}