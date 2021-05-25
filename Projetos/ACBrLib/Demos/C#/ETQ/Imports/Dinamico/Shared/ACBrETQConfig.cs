using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.ETQ;

namespace ACBrLib.ETQ
{
    public sealed class ACBrETQConfig : ACBrLibConfig<ACBrETQ>
    {
        #region Constructors

        public ACBrETQConfig(ACBrETQ acbretq) : base(acbretq, ACBrSessao.ETQ)
        {
            Device = new DeviceConfig<ACBrETQ>(acbretq, ACBrSessao.ETQ_Device);
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Retorna configurações da DANFe.
        /// </summary>
        public DeviceConfig<ACBrETQ> Device { get; }

        public string ArqLog
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Porta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int Temperatura
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Velocidade
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Avanco
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int MargemEsquerda
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool LimparMemoria
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool Ativo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public ETQPageCode PaginaDeCodigo
        {
            get => GetProperty<ETQPageCode>();
            set => SetProperty(value);
        }

        public ETQModelo Modelo
        {
            get => GetProperty<ETQModelo>();
            set => SetProperty(value);
        }

        public ETQUnidade Unidade
        {
            get => GetProperty<ETQUnidade>();
            set => SetProperty(value);
        }

        public ETQBackFeed BackFeed
        {
            get => GetProperty<ETQBackFeed>();
            set => SetProperty(value);
        }

        public ETQOrigem Origem
        {
            get => GetProperty<ETQOrigem>();
            set => SetProperty(value);
        }

        public ETQDPI DPI
        {
            get => GetProperty<ETQDPI>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}