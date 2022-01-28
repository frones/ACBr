using System;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using ACBrLib.Core.Extensions;

namespace ACBrLib.Core
{
    public static class IniValueWrapper
    {
        #region Properties

        public static NumberFormatInfo NumberFormatInfo { get; }

        #endregion Properties

        #region Constructor

        static IniValueWrapper()
        {
            NumberFormatInfo = new NumberFormatInfo() { NumberGroupSeparator = ".", NumberDecimalSeparator = "," };
        }

        #endregion Constructor

        #region Methods

        public static string Wrap<T>(object value)
        {
            return Wrap(typeof(T), value);
        }

        public static T UnWrap<T>(string value, T defaultValue = default)
        {
            return (T)UnWrap(typeof(T), value, defaultValue);
        }

        public static string Wrap(Type type, object value)
        {
            if (!CanWrapUnwrap(type)) throw new ArgumentException("Apenas tipos primitivos podem ser convertidos.");
            if (value == null) return null;

            switch (value)
            {
                case decimal dValue: return dValue.ToString(NumberFormatInfo);

                case double dbValue: return dbValue.ToString(NumberFormatInfo);

                case float fValue: return fValue.ToString(NumberFormatInfo);

                case DateTime dtValue: return dtValue.TimeOfDay.TotalSeconds == 0 ? $"{dtValue:dd/MM/yyyy}" : $"{dtValue:dd/MM/yyyy HH:mm:ss}";

                case TimeSpan tmValue: return $"{tmValue:HH:mm:ss}";

                case bool boValue: return boValue ? "1" : "0";

                case Enum _:
                    var member = value.GetType().GetMember(value.ToString()).FirstOrDefault();
                    var enumAttribute = member?.GetCustomAttributes(false).OfType<EnumValueAttribute>().FirstOrDefault();
                    var enumValue = enumAttribute?.Value;
                    return enumValue ?? Convert.ToInt32(value).ToString();

                case string[] aString:
                    return string.Join("|", aString);

                case Stream aStream:
                    return Convert.ToBase64String(aStream.ReadAllBytes());

                default:
                    return value.ToString();
            }
        }

        public static object UnWrap(Type type, string value, object defaultValue)
        {
            if (!CanWrapUnwrap(type)) throw new ArgumentException("Apenas tipos primitivos podem ser convertidos.");
            if (string.IsNullOrEmpty(value)) return defaultValue;

            if (type == typeof(decimal) || type == typeof(decimal?))
            {
                decimal.TryParse(value, NumberStyles.Number, NumberFormatInfo, out var ret);
                return ret;
            }

            if (type == typeof(double) || type == typeof(double?))
            {
                double.TryParse(value, NumberStyles.Number, NumberFormatInfo, out var ret);
                return ret;
            }

            if (type == typeof(float) || type == typeof(float?))
            {
                float.TryParse(value, NumberStyles.Number, NumberFormatInfo, out var ret);
                return ret;
            }

            if (type == typeof(DateTime) || type == typeof(DateTime?))
            {
                DateTime.TryParseExact(value, new[] { "dd/MM/yyyy", "dd/MM/yyyy HH:mm:ss" }, null,
                    DateTimeStyles.AssumeLocal, out var ret);
                return ret;
            }

            if (type == typeof(TimeSpan) || type == typeof(TimeSpan?))
            {
                TimeSpan.TryParseExact(value, "HH:mm:ss", null, out var ret);
                return ret;
            }

            if (type == typeof(bool) || type == typeof(bool?)) return value == "1";

            if (type == typeof(int) || type == typeof(int?))
            {
                int.TryParse(value, out var ret);
                return ret;
            }

            if (type == typeof(string[])) return value.Split('|');

            if (type == typeof(Stream))
            {
                var ret = new MemoryStream();
                var pdfBytes = Convert.FromBase64String(value);
                ret.Write(pdfBytes, 0, pdfBytes.Length);
                ret.Flush();

                ret.Position = 0;
            }

            if (!type.IsSubclassOf(typeof(Enum))) return value;

            var enumType = type.IsGenericType ? type.GetGenericArguments()[0] : type;
            object enumValue = enumType.GetMembers().Where(x => x.HasAttribute<EnumValueAttribute>())
                .SingleOrDefault(x => x.GetAttribute<EnumValueAttribute>().Value == value)?.Name;

            return enumValue == null ? Enum.ToObject(enumType, Convert.ToInt32(value)) :
                                       Enum.Parse(enumType, enumValue.ToString());
        }

        public static bool CanWrapUnwrap(Type type)
        {
            return type == typeof(string)
                   || type == typeof(char)
                   || type == typeof(sbyte)
                   || type == typeof(short)
                   || type == typeof(int)
                   || type == typeof(long)
                   || type == typeof(byte)
                   || type == typeof(ushort)
                   || type == typeof(uint)
                   || type == typeof(ulong)
                   || type == typeof(double)
                   || type == typeof(float)
                   || type == typeof(decimal)
                   || type == typeof(bool)
                   || type == typeof(IntPtr)
                   || type == typeof(UIntPtr)
                   || type == typeof(DateTime)
                   || type == typeof(DateTimeOffset)
                   || type == typeof(TimeSpan)
                   || type == typeof(Stream)
                   || type.IsEnum
                   || type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>) && CanWrapUnwrap(type.GetGenericArguments()[0]);
        }

        #endregion Methods
    }
}