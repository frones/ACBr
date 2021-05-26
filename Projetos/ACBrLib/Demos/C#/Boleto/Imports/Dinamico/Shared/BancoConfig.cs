using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    /// <summary>
    /// Configurações da Sessão [BoletoBancoConfig]
    /// </summary>
    public sealed class BancoConfig : ACBrLibConfigBase<ACBrBoleto>
    {
        #region Constructors

        /// <summary>
        /// Inicializa uma nova instancia da classe  <see cref="BancoConfig"/>.
        /// </summary>
        /// <param name="acbrboleto">Instancia do ACBrBoleto</param>
        public BancoConfig(ACBrBoleto acbrboleto) : base(acbrboleto, ACBrSessao.BoletoBancoConfig)
        {
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Define qual o Banco será utilizado.
        /// É necessário a definição do Banco antes de utilizar qualquer método do componente Boleto.
        /// </summary>
        public ACBrTipoCobranca TipoCobranca
        {
            get => GetProperty<ACBrTipoCobranca>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O código utilizado pelo Banco.
        /// Caso não informado será preenchido automaticamente pelo componente, baseado nas configurações da classe do banco selecionado em "TipoCobrança"
        /// </summary>
        public int Numero
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O Código do Correspondente Bancário utilizado pelo Banco.
        /// Utilizado apenas por algumas cooperativas de crédito.
        /// </summary>
        public int NumeroCorrespondente
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O Número da Versão do Layout do arquivo de Remessa.
        /// Não preencher se não necessário.
        /// </summary>
        public int LayoutVersaoArquivo
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O Número da Versão do Lote do arquivo de Remessa.
        /// Não preencher se não necessário.
        /// </summary>
        public int LayoutVersaoLote
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o local de pagamento a ser impresso no boleto desse banco.
        /// </summary>
        public string LocalPagamento
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define uma string com as orientações do Banco para ser impresso no Boleto
        /// </summary>
        public string OrientacaoBanco
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O Dígito do Banco.
        /// Não preencher se não existir.
        /// </summary>
        public int Digito
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}