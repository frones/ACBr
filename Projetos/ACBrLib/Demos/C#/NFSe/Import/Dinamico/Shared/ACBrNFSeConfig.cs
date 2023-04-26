using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Config;
using ACBrLib.Core.NFSe;

namespace ACBrLib.NFSe
{
    public sealed class ACBrNFSeConfig : ACBrLibDFeConfig<ACBrNFSe>
    {
        #region Constructors

        public ACBrNFSeConfig(ACBrNFSe acbrlib) : base(acbrlib, ACBrSessao.NFSe)
        {
            DANFSe = new DANFSeConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public DANFSeConfig DANFSe { get; }

        public string PathNFSe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool AtualizarXMLCancelado
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool MontarPathSchema
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ConsultaLoteAposEnvio
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ConsultaAposCancelar
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public LayoutNFSe LayoutNFSe
        {
            get => GetProperty<LayoutNFSe>();
            set => SetProperty(value);
        }

        public bool EmissaoPathNFSe
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public CodigoMunicipio CodigoMunicipio
        {
            get => GetProperty<CodigoMunicipio>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}