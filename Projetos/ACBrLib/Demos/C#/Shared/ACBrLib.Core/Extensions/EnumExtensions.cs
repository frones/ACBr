using System;
using System.Collections.Generic;
using System.Linq;

namespace ACBrLib.Core.Extensions
{
    public static class EnumExtensions
    {
        public static string GetEnumValue<TEnum>(this TEnum eValue, bool useIntOnFlag = false) where TEnum : Enum
        {
            var enumType = typeof(TEnum).IsGenericType ? typeof(TEnum).GetGenericArguments()[0] : typeof(TEnum);

            if (Attribute.IsDefined(enumType, typeof(FlagsAttribute)))
            {
                var member = enumType.GetMember(eValue.ToString()).First();

                if (!Attribute.IsDefined(member, typeof(EnumValueAttribute)))
                    return useIntOnFlag ? Convert.ToInt32(eValue).ToString() : $"[{Enum.Format(enumType, eValue, "F")}]";

                var flags = eValue.GetFlagValues();
                return flags.Aggregate("[", (current, enValue) =>
                {
                    if (current.Length > 1) current += ",";

                    var eMember = enumType.GetMember(enValue.ToString()).First();
                    var eAtt = eMember.GetCustomAttributes(false).OfType<EnumValueAttribute>().First();
                    if (eAtt == null) throw new ArgumentException("Tipo de enum [EnumValue], sem valores definidos.");

                    return current + eAtt.Value;
                }, current => current + "]");
            }
            else
            {
                var member = enumType.GetMember(eValue.ToString()).First();
                var enumAttribute = member.GetCustomAttributes(false).OfType<EnumValueAttribute>().FirstOrDefault();
                return enumAttribute?.Value ?? Convert.ToInt32(eValue).ToString();
            }
        }

        public static IEnumerable<TEnum> GetFlagValues<TEnum>(this TEnum flags) where TEnum : Enum
        {
            if (!Attribute.IsDefined(typeof(TEnum), typeof(FlagsAttribute))) throw new ArgumentException("Função apenas para enuns do tipo Flag.");

            return from Enum value in Enum.GetValues(flags.GetType()) where flags.HasFlag(value) select (TEnum)value;
        }
    }
}