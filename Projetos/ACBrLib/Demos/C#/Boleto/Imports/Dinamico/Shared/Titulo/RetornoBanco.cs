using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class RetornoBanco
    {
        #region Properties

        public int Numero { get; set; }

        [IniKey("IndiceACBr")]
        public ACBrTipoCobranca TipoCobranca { get; set; }

        public int NumeroCorrespondente { get; set; }

        public int VersaoArquivo { get; set; }

        public int VersaoLote { get; set; }

        public int NumeroArquivo { get; set; }

        #endregion Properties
    }
}