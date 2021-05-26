using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.Sat;

namespace ACBrLib.Sat
{
    public sealed class SATRede : ACBrLibConfigBase<ACBrSat>
    {
        #region Constructors

        public SATRede(ACBrSat acbrlib) : base(acbrlib, ACBrSessao.SATRede)
        {
        }

        #endregion Constructors

        #region Properties

        public TipoInterface tipoInter
        {
            get => GetProperty<TipoInterface>();
            set => SetProperty(value);
        }

        public string SSID
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public SegSemFio seg
        {
            get => GetProperty<SegSemFio>();
            set => SetProperty(value);
        }

        public string codigo
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public TipoLan tipoLan
        {
            get => GetProperty<TipoLan>();
            set => SetProperty(value);
        }

        public string lanIP
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string lanMask
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string lanGW
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string lanDNS1
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string lanDNS2
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Usuario
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Senha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}