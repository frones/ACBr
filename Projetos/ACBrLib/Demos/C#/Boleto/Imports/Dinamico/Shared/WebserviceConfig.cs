using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    public sealed class WebserviceConfig : ACBrLibConfigBase<ACBrBoleto>
    {
        #region Constructors

        public WebserviceConfig(ACBrBoleto acbrlib) : base(acbrlib, ACBrSessao.BoletoWebSevice)
        {
        }

        #endregion Constructors

        #region Properties

        public string LogRegistro
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathGravarRegistro
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public AmbienteWebservice Ambiente
        {
            get => GetProperty<AmbienteWebservice>();
            set => SetProperty(value);
        }

        public OperacaoBoleto Operação
        {
            get => GetProperty<OperacaoBoleto>();
            set => SetProperty(value);
        }

        public string VersaoDF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool UseCertificateHTTP
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}