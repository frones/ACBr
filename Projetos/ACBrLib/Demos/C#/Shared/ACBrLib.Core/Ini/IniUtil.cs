using System;
using System.Globalization;
using System.Linq;
using System.Reflection;
using ACBrLib.Core.Extensions;

namespace ACBrLib.Core.Ini
{
    public static class IniUtil
    {
        #region Properties

        public static NumberFormatInfo NumberFormatInfo { get; }

        #endregion Properties

        #region Constructor

        static IniUtil()
        {
            NumberFormatInfo = new NumberFormatInfo() { NumberGroupSeparator = "", NumberDecimalSeparator = "." };
        }

        #endregion Constructor

        #region Methods

        private static string ToString(decimal value)
        {
            return string.Format(NumberFormatInfo, "{0:n2}", value);
        }

        private static string ToString(float value)
        {
            return string.Format(NumberFormatInfo, "{0:n2}", value);
        }

        private static string ToString(double value)
        {
            return string.Format(NumberFormatInfo, "{0:n2}", value);
        }

        private static string ToString(DateTime value)
        {
            return value.TimeOfDay.TotalSeconds == 0 ? $"{value:dd/MM/yyyy}" : $"{value:dd/MM/yyyy HH:mm:ss}";
        }

        private static string ToString(bool value)
        {
            return value ? "True" : "False";
        }

        private static string ToString(int value)
        {
            return value.ToString();
        }

        private static int ToInt32(string value)
        {
            int.TryParse(value, out var ret);
            return ret;
        }

        private static decimal ToDecimal(string value)
        {
            decimal.TryParse(value, NumberStyles.Number, NumberFormatInfo, out var ret);
            return ret;
        }

        private static bool ToBoolean(string value)
        {
            return value == "True";
        }

        private static float ToFloat(string value)
        {
            float.TryParse(value, NumberStyles.Number, NumberFormatInfo, out var ret);
            return ret;
        }

        private static double ToDouble(string value)
        {
            double.TryParse(value, NumberStyles.Number, NumberFormatInfo, out var ret);
            return ret;
        }

        private static DateTime ToDateTime(string value)
        {
            DateTime.TryParseExact(value, new[] { "dd/MM/yyyy", "dd/MM/yyyy HH:mm:ss" }, null, DateTimeStyles.AssumeLocal, out var ret);
            return ret;
        }

        private static bool IsPrimitive(Type type)
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
                   || type.IsEnum
                   || type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>) && IsPrimitive(type.GetGenericArguments()[0]);
        }

        public static void WriteToIni<T>(this ACBrIniFile iniData, T obj, string sectionName) where T : class
        {
            if (obj == null) return;

            var sectionData = new ACBrIniSection(sectionName);
            sectionData.WriteToIni(obj);
            if (sectionData.Count > 0)
                iniData.Add(sectionData);
        }

        public static void WriteToIni<T>(this ACBrIniSection section, T obj) where T : class
        {
            foreach (var property in typeof(T).GetProperties(BindingFlags.Public | BindingFlags.Instance))
            {
                if (!(property.CanRead && property.CanWrite && IsPrimitive(property.PropertyType))) continue;

                var value = property.GetValue(obj, null);
                if (value == null) continue;

                string str;

                switch (value)
                {
                    case string stValue:
                        str = stValue;
                        break;

                    case decimal dValue:
                        str = ToString(dValue);
                        break;

                    case double dbValue:
                        str = ToString(dbValue);
                        break;

                    case float fValue:
                        str = ToString(fValue);
                        break;

                    case DateTime dtValue:
                        str = ToString(dtValue);
                        break;

                    case bool boValue:
                        str = ToString(boValue);
                        break;

                    case Enum _:
                        var member = value.GetType().GetMember(value.ToString()).FirstOrDefault();
                        var enumAttribute = member?.GetCustomAttributes(false).OfType<EnumValueAttribute>().FirstOrDefault();
                        var enumValue = enumAttribute?.Value;
                        str = enumValue ?? ToString(Convert.ToInt32(value));
                        break;

                    default:
                        str = value.ToString();
                        break;
                }

                if (!string.IsNullOrEmpty(str))
                    section.Add(property.Name, str);
            }
        }

        public static T ReadFromIni<T>(this ACBrIniFile iniData, string secionName) where T : class, new()
        {
            if (!iniData.Contains(secionName)) return null;
            var section = iniData[secionName];

            var ret = Activator.CreateInstance<T>();
            section.ReadFromINi(ret);
            return ret;
        }

        public static void ReadFromIni<T>(this ACBrIniFile iniData, T obj, string secionName) where T : class
        {
            if (!iniData.Contains(secionName)) return;
            var section = iniData[secionName];

            section.ReadFromINi(obj);
        }

        public static void ReadFromINi<T>(this ACBrIniSection section, T item) where T : class
        {
            foreach (var property in typeof(T).GetProperties(BindingFlags.Public | BindingFlags.Instance))
            {
                if (!(property.CanRead && property.CanWrite && IsPrimitive(property.PropertyType))) continue;
                if (!section.ContainsKey(property.Name)) continue;

                var str = section[property.Name];
                if (str == null) continue;

                object value;

                if (property.PropertyType == typeof(decimal) || property.PropertyType == typeof(decimal?))
                {
                    value = ToDecimal(str);
                }
                else if (property.PropertyType == typeof(double) || property.PropertyType == typeof(double?))
                {
                    value = ToDouble(str);
                }
                else if (property.PropertyType == typeof(float) || property.PropertyType == typeof(float?))
                {
                    value = ToFloat(str);
                }
                else if (property.PropertyType == typeof(DateTime) || property.PropertyType == typeof(DateTime?))
                {
                    value = ToDateTime(str);
                }
                else if (property.PropertyType == typeof(bool) || property.PropertyType == typeof(bool?))
                {
                    value = ToBoolean(str);
                }
                else if (property.PropertyType == typeof(int) || property.PropertyType == typeof(int?))
                {
                    value = ToInt32(str);
                }
                else if (property.PropertyType.IsSubclassOf(typeof(Enum)))
                {
                    var enumType = property.PropertyType.IsGenericType ? property.PropertyType.GetGenericArguments()[0] : property.PropertyType;
                    object value1 = enumType.GetMembers().Where(x => x.HasAttribute<EnumValueAttribute>())
                        .SingleOrDefault(x => x.GetAttribute<EnumValueAttribute>().Value == str)?.Name;

                    value = value1 == null ? Enum.ToObject(enumType, Convert.ToInt32(str)) :
                                             Enum.Parse(enumType, value1.ToString());
                }
                else
                {
                    value = str;
                }

                property.SetValue(item, value, null);
            }
        }

        #endregion Methods
    }
}