namespace ACBrLib.ETQ
{
    public sealed partial class ACBrETQ
    {
        private ACBrETQConfig config;

        public ACBrETQConfig Config => config ?? (config = new ACBrETQConfig(this));
    }
}