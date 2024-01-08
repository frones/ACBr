using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    /// <summary>
    /// Configurações da Sessão [BoletoDiretorioConfig]
    /// </summary>
    public sealed class CedenteConfig : ACBrLibConfigBase<ACBrBoleto>
    {
        #region Constructors

        /// <summary>
        /// Inicializa uma nova instancia da classe  <see cref="CedenteConfig"/>.
        /// </summary>
        /// <param name="acbrboleto">Instancia do ACBrBoleto</param>
        public CedenteConfig(ACBrBoleto acbrboleto) : base(acbrboleto, ACBrSessao.BoletoCedenteConfig)
        {
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Define o número da agência do cedente.
        /// </summary>
        public string Agencia
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o número do dígito da agência.
        /// </summary>
        public string AgenciaDigito
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o número da conta do cedente.
        /// </summary>
        public string Conta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o número do digito da agência.
        /// </summary>
        public string ContaDigito
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o digito verificador da agência e conta.
        /// Utilizado apenas por alguns bancos.
        /// </summary>
        public string DigitoVerificadorAgenciaConta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o número do convênio, deve ser verificado com o banco.
        /// </summary>
        public string Convenio
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o código do cedente, deve ser verificado com o banco.
        /// </summary>
        public string CodigoCedente
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o código de transmissão, deve ser verificado com o Banco, quando existir.
        /// </summary>
        public string CodigoTransmissao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o código da modalidade, deve ser verificado com o Banco, quando existir.
        /// </summary>
        public string Modalidade
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o responsável pela emissão do boleto.
        /// </summary>
        public ACBrResponEmissao ResponEmissao
        {
            get => GetProperty<ACBrResponEmissao>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o responsável pela emissão do boleto.
        /// </summary>
        public ACBrTipoCarteira TipoCarteira
        {
            get => GetProperty<ACBrTipoCarteira>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o tipo de documento.
        /// </summary>
        public ACBrTipoDocumento TipoDocumento
        {
            get => GetProperty<ACBrTipoDocumento>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o tipo de inscrição.
        /// </summary>
        public ACBrPessoa TipoInscricao
        {
            get => GetProperty<ACBrPessoa>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define características de cobrança do título.
        /// </summary>
        public CaracTitulo CaracTitulo
        {
            get => GetProperty<CaracTitulo>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O número do CNPJ ou CPF do cedente.
        /// </summary>
        public string CNPJCPF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O nome do cedente.
        /// </summary>
        public string Nome
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O endereço do cedente.
        /// </summary>
        public string Logradouro
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O número residencial do cedente.
        /// </summary>
        public string NumeroRes
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O complemento de endereço do cedente.
        /// </summary>
        public string Complemento
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O bairro do cedente.
        /// </summary>
        public string Bairro
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// A cidade do cedente.
        /// </summary>
        public string Cidade
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O CEP do cedente.
        /// </summary>
        public string CEP
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// A UF do cedente.
        /// </summary>
        public string UF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// O telefone do cedente.
        /// </summary>
        public string Telefone
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define quem ira emitir o boleto.
        /// </summary>
        public IdentDistribuicao IdentDistribuicao
        {
            get => GetProperty<IdentDistribuicao>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Dados adicionais do cedente.
        /// </summary>
        public string Operacao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ChavePIX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public TipoChavePIX TipoChavePIX
        {
            get => GetProperty<TipoChavePIX>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}