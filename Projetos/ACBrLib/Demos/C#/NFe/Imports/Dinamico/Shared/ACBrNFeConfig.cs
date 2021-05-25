using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class ACBrNFeConfig : ACBrLibDFeConfig<ACBrNFe>
    {
        #region Constructors

        public ACBrNFeConfig(ACBrNFe acbrnfe) : base(acbrnfe, ACBrSessao.NFe)
        {
            DANFe = new DANFeConfig(acbrnfe);
            PosPrinter = new PosPrinterConfig<ACBrNFe>(acbrnfe);
            Config = new DownloadConfig<ACBrNFe>(acbrnfe, ACBrSessao.NFe);
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Retorna configurações da DANFe.
        /// </summary>
        public DANFeConfig DANFe { get; }

        public PosPrinterConfig<ACBrNFe> PosPrinter { get; }

        public TipoEmissao FormaEmissao
        {
            get => GetProperty<TipoEmissao>();
            set => SetProperty(value);
        }

        public bool SalvarGer
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ExibirErroSchema
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string FormatoAlerta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool RetirarAcentos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool RetirarEspacos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool IdentarXML
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ValidarDigest
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string IdCSC
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string CSC
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public ModeloNFe ModeloDF
        {
            get => GetProperty<ModeloNFe>();
            set => SetProperty(value);
        }

        public VersaoNFe VersaoDF
        {
            get => GetProperty<VersaoNFe>();
            set => SetProperty(value);
        }

        public bool AtualizarXMLCancelado
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public VersaoQrCode VersaoQRCode
        {
            get => GetProperty<VersaoQrCode>();
            set => SetProperty(value);
        }

        public bool CamposFatObrigatorios
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public TagNT2018005 TagNT2018005
        {
            get => GetProperty<TagNT2018005>();
            set => SetProperty(value);
        }

        public TipoAmbiente Ambiente
        {
            get => GetProperty<TipoAmbiente>();
            set => SetProperty(value);
        }

        public bool SalvarWS
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int Timeout
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TimeoutPorThread
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool Visualizar
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool AjustaAguardaConsultaRet
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int AguardarConsultaRet
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int IntervaloTentativas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Tentativas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public SSLType SSLType
        {
            get => GetProperty<SSLType>();
            set => SetProperty(value);
        }

        public string QuebradeLinha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathSalvar
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathSchemas
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string IniServicos
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool SalvarArq
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool AdicionarLiteral
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorCNPJ
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorModelo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorAno
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorMes
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorDia
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SalvarEvento
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SalvarApenasNFeProcessadas
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool EmissaoPathNFe
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool NormatizarMunicipios
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PathNFe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathInu
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathEvento
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathArquivoMunicipios
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public DownloadConfig<ACBrNFe> Config { get; }

        public string IdCSRT
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string CSRT
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}