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
            Matera = new MateraConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public MateraConfig Matera { get; }

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