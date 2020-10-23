namespace ACBr.PDV.Model
{
    public class RegistroBobina
    {
        #region Constructors

        public RegistroBobina(string linha1, string linha2 = "")
        {
            Linha1 = linha1;
            Linha2 = linha2;
        }

        public RegistroBobina()
        {
            Linha1 = "".PadLeft(48, ' ');
            Linha2 = "".PadLeft(48, ' ');
        }

        #endregion Constructors

        #region Properties

        public string Linha1 { get; set; }

        public string Linha2 { get; set; }

        #endregion Properties
    }
}