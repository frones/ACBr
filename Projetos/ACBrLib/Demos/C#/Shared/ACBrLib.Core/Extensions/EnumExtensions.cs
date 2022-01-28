using System;
using System.Collections.Generic;
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

        public static IEnumerable<TEnum> GetFlagValues<TEnum>(this TEnum flags) where TEnum : Enum
        {
            return from Enum value in Enum.GetValues(flags.GetType()) where flags.HasFlag(value) select (TEnum)value;
        }
    }
}