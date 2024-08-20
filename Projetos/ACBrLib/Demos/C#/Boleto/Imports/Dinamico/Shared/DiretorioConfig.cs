using System;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    /// <summary>
    /// Configurações da Sessão [BoletoCedenteConfig]
    /// </summary>
    public sealed class DiretorioConfig : ACBrLibConfigBase<ACBrBoleto>
    {
        #region Constructors

        /// <summary>
        /// Inicializa uma nova instancia da classe  <see cref="DiretorioConfig"/>.
        /// </summary>
        /// <param name="acbrboleto">Instancia do ACBrBoleto</param>
        public DiretorioConfig(ACBrBoleto acbrboleto) : base(acbrboleto, ACBrSessao.BoletoDiretorioConfig)
        {
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Define o modelo do CNAB para geração da remessa.
        /// </summary>
        public ACBrLayoutRemessa LayoutRemessa
        {
            get => GetProperty<ACBrLayoutRemessa>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o diretório para geração do arquivo de remessa.
        /// </summary>
        public string DirArqRemessa
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o diretório para leitura do arquivo de retorno.
        /// </summary>
        public string DirArqRetorno
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o prefixo do nome do arquivo de remessa a ser gerado pelo componente boleto.
        /// </summary>
        public string PrefixArqRemessa
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o nome do arquivo de remessa a ser gerado pelo componente boleto.
        /// </summary>
        public string NomeArqRemessa
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o nome do arquivo de retorno a ser lido pelo componente boleto.
        /// </summary>
        public string NomeArqRetorno
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o numero sequencial do arquivo remessa a ser gerado.
        /// Também pode ser parametrizado via comando.
        /// </summary>
        public int NumeroArquivo
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Data de geração do Arquivo de Remessa.
        /// Não preencher se não utilizado.
        /// </summary>
        public DateTime DataArquivo
        {
            get => GetProperty<DateTime>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Data de créditos de lançamento do Arquivo de Retorno.
        /// Não preencher se não utilizado.
        /// </summary>
        public DateTime DataCreditoLanc
        {
            get => GetProperty<DateTime>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define a impressão ou não de mensagens padrão no boleto.
        /// </summary>
        public bool ImprimirMensagemPadrao
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define se lê os dados do cedente para validar o arquivo de retorno.
        /// </summary>
        public bool LeCedenteRetorno
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define se deve ou não remover os acentos no arquivo de remessa.
        /// </summary>
        public bool RemoveAcentosArqRemessa
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define se lê o campo nosso numero completo.
        /// </summary>
        public bool LerNossoNumeroCompleto
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }
        #endregion Properties
    }
}