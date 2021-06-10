using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class Ocorrencia
    {
        #region Properties

        [IniKey("OcorrenciaOriginal.TipoOcorrencia")]
        public TipoOcorrencia Tipo { get; set; }

        #endregion Properties
    }
}