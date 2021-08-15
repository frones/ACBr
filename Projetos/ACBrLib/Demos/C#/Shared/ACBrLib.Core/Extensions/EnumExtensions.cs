using System;
using System.Linq;

namespace ACBrLib.Core.Extensions
{
    public static class EnumExtensions
    {
        public static string GetEnumValueOrInt<TEnum>(this TEnum evento) where TEnum : Enum
        {
            var member = evento.GetType().GetMember(evento.ToString()).FirstOrDefault();
            var enumAttribute = member?.GetCustomAttributes(false).OfType<EnumValueAttribute>().FirstOrDefault();
            var enumValue = enumAttribute?.Value;
            return enumValue ?? Convert.ToInt32(evento).ToString();
        }
    }
}