using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFSe;

namespace ACBrLib.NFSe
{
    public sealed class DANFSeConfig : ReportConfig<ACBrNFSe>
    {
        #region Constructors

        public DANFSeConfig(ACBrNFSe acbrlib) : base(acbrlib, ACBrSessao.DANFSe)
        {

        }

        #endregion Constructors

        #region Properties
        
        public TipoDANFSE TipoDANFSE
        {
            get => GetProperty<TipoDANFSE>();
            set => SetProperty(value);
        }

        public string Prefeitura
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool Cancelada
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }
        #endregion Properties
    }
}