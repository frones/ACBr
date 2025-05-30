namespace ACBrLib.Core.Serial
{
    public enum SerialParity
    {
        [EnumValue("N")]
        None,

        [EnumValue("O")]
        Odd,

        [EnumValue("E")]
        Even,

        [EnumValue("M")]
        Mark,

        [EnumValue("S")]
        Space
    }
}