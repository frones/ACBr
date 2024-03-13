using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.DFe;
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

        public AmbienteWebservice Ambiente
        {
            get => GetProperty<AmbienteWebservice>();
            set => SetProperty(value);
        }

        public OperacaoBoleto Operacao
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

        public SSLType SSLType
        {
            get => GetProperty<SSLType>();
            set => SetProperty(value);
        } 

        public int Timeout
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public string ArquivoCRT
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArquivoKEY
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathGravarRegistro
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string NomeArquivoLog
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public NivelLog LogNivel
        {
            get => GetProperty<NivelLog>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}