using ACBrLib.Core;
using ACBrLib.Core.Config;
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
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Retorna configurações da DANFe.
        /// </summary>
        public DANFeConfig DANFe { get; }

        public PosPrinterConfig<ACBrNFe> PosPrinter { get; }

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