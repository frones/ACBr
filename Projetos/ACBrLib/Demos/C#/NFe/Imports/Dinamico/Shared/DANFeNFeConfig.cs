using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class DANFeNFeConfig : ACBrLibConfigBase<ACBrNFe>
    {
        #region Constructors

        public DANFeNFeConfig(ACBrNFe acbrlib) : base(acbrlib, ACBrSessao.DANFENFe)
        {
            Fonte = new FontConfig<ACBrNFe>(acbrlib, ACBrSessao.DANFENFe);
        }

        #endregion Constructors

        #region Properties

        public bool FormularioContinuo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public ImprimeValor ImprimeValor
        {
            get => GetProperty<ImprimeValor>();
            set => SetProperty(value);
        }

        public bool ImprimeDescPorPercentual
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeDetalhamentoEspecifico
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public PosCanhoto PosCanhoto
        {
            get => GetProperty<PosCanhoto>();
            set => SetProperty(value);
        }

        public bool ExibeResumoCanhoto
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string TextoResumoCanhoto
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool ExibeCampoFatura
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ExibeDadosISSQN
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ExibeDadosDocReferenciados
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public DetVeiculos DetVeiculos
        {
            get => GetProperty<DetVeiculos>();
            set => SetProperty(value);
        }

        public DetMedicamentos DetMedicamentos
        {
            get => GetProperty<DetMedicamentos>();
            set => SetProperty(value);
        }

        public DetArmamentos DetArmamentos
        {
            get => GetProperty<DetArmamentos>();
            set => SetProperty(value);
        }

        public DetCombustiveis DetCombustiveis
        {
            get => GetProperty<DetCombustiveis>();
            set => SetProperty(value);
        }

        public DetRastros DetRastros
        {
            get => GetProperty<DetRastros>();
            set => SetProperty(value);
        }

        public TributosPercentual TributosPercentual
        {
            get => GetProperty<TributosPercentual>();
            set => SetProperty(value);
        }

        public string TributosPercentualPersonalizado
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string MarcadAgua
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int LarguraCodProd
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool ExibeEAN
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int AltLinhaComun
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int EspacoEntreProdutos
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool AlternaCoresProdutos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string CorDestaqueProdutos
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int TamanhoLogoHeight
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TamanhoLogoWidth
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int RecuoEndereco
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int RecuoEmpresa
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool LogoemCima
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int RecuoLogo
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool ExpandirDadosAdicionaisAuto
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeContDadosAdPrimeiraPagina
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public FontConfig<ACBrNFe> Fonte { get; }

        public PosCanhotoLayout PosCanhotoLayout
        {
            get => GetProperty<PosCanhotoLayout>();
            set => SetProperty(value);
        }

        public ExibeCampoDePagamento ExibeCampoDePagamento
        {
            get => GetProperty<ExibeCampoDePagamento>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}