namespace ACBr.PDV.Model
{
    public class DocumentoFiscal
    {
        #region Constructors

        public DocumentoFiscal()
        {
            TipoDocumento = TipoDFe.SAT;
        }

        #endregion Constructors

        #region Properties

        public TipoDFe TipoDocumento { get; set; }

        public int Serie { get; set; }

        public int NumeroAtual { get; set; }

        #endregion Properties

        #region Methods

        public void Clear()
        {
            TipoDocumento = TipoDFe.SAT;
            Serie = 0;
            NumeroAtual = 0;
        }

        #endregion Methods
    }
}