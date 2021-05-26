using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    /// <summary>
    /// Configurações da Sessão [BoletoBancoFCFortesConfig]
    /// </summary>
    public sealed class ImpressaoConfig : ACBrLibConfigBase<ACBrBoleto>
    {
        #region Constructors

        /// <summary>
        /// Inicializa uma nova instancia da classe  <see cref="ImpressaoConfig"/>.
        /// </summary>
        /// <param name="acbrboleto">Instancia do ACBrBoleto</param>
        public ImpressaoConfig(ACBrBoleto acbrboleto) : base(acbrboleto, ACBrSessao.BoletoBancoFCFortesConfig)
        {
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Define o nome do arquivo para geração do Boleto em PDF.
        /// Informar o caminho completo ex: c:\temp\BoletoPDF.pdf.
        /// </summary>
        public string NomeArquivo
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o diretório onde está localizado os logotipos dos bancos.
        /// </summary>
        public string DirLogo
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o filtro para geração do boleto.
        /// </summary>
        public ACBrBoletoFiltro Filtro
        {
            get => GetProperty<ACBrBoletoFiltro>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o tipo de layout para impressão do boleto.
        /// </summary>
        public ACBrBolLayOut Layout
        {
            get => GetProperty<ACBrBolLayOut>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define a se exibe o preview.
        /// </summary>
        public bool MostrarPreview
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define a se exibe o progresso da geração do relatorio.
        /// </summary>
        public bool MostrarProgresso
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define a se exibe a tela de opções de impressão.
        /// </summary>
        public bool MostrarSetup
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o número de copias.
        /// </summary>
        public int NumeroCopias
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define a impressora onde será impresso.
        /// </summary>
        public string PrinterName
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        /// <summary>
        /// Define o nome da empresa a ser impresso no boleto.
        /// </summary>
        public string SoftwareHouse
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}