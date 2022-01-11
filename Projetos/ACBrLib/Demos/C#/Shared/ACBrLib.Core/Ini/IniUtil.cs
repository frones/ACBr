using System;
using System.Linq;
using System.Reflection;
using ACBrLib.Core.Extensions;

namespace ACBrLib.Core
{
    public static class IniUtil
    {
        #region Methods

        public static void WriteToIni<T>(this ACBrIniFile iniData, T obj, string sectionName) where T : class
        {
            if (obj == null) return;

            iniData.WriteToIni(typeof(T), obj, sectionName);
        }

        public static void WriteToIni(this ACBrIniFile iniData, Type tipo, object obj, string sectionName)
        {
            if (obj == null) return;

            var sectionData = iniData.SingleOrDefault(x => x.Name == sectionName) ?? new ACBrIniSection(sectionName);
            sectionData.WriteToIni(tipo, obj);

            if (sectionData.Count > 0 && !iniData.Contains(sectionName))
                iniData.Add(sectionData);
        }

        public static void WriteToIni<T>(this ACBrIniSection section, T obj) where T : class
        {
            section.WriteToIni(typeof(T), obj);
        }

        public static void WriteToIni(this ACBrIniSection section, Type tipo, object obj)
        {
            if (!tipo.IsClass) return;

            foreach (var property in tipo.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.NonPublic))
            {
                if (property.HasAttribute<IniIgnoreAttribute>()) continue;
                if (!(property.CanRead && property.CanWrite)) continue;
                if (!IniValueWrapper.CanWrapUnwrap(property.PropertyType)) continue;

                var value = property.GetValue(obj, null);
                if (value == null) continue;

                var str = IniValueWrapper.Wrap(property.PropertyType, value);
                var keyName = property.HasAttribute<IniKeyAttribute>() ? property.GetAttribute<IniKeyAttribute>().Value : property.Name;
                if (!string.IsNullOrEmpty(str))
                    section.Add(keyName, str);
            }
        }

        public static T ReadFromIni<T>(this ACBrIniFile iniData, string secionName) where T : class, new()
        {
            return (T)iniData.ReadFromIni(typeof(T), secionName);
        }

        public static void ReadFromIni<T>(this ACBrIniFile iniData, T obj, string secionName) where T : class
        {
            if (!iniData.Contains(secionName)) return;
            var section = iniData[secionName];

            section.ReadFromINi(obj);
        }

        public static object ReadFromIni(this ACBrIniFile iniData, Type tipo, string secionName)
        {
            if (!iniData.Contains(secionName)) return null;
            var section = iniData[secionName];

            var ret = Activator.CreateInstance(tipo);
            section.ReadFromINi(tipo, ret);
            return ret;
        }

        public static T ReadFromINi<T>(this ACBrIniSection section) where T : class
        {
            var ret = (T)Activator.CreateInstance(typeof(T));
            section.ReadFromINi(typeof(T), ret);
            return ret;
        }

        public static void ReadFromINi<T>(this ACBrIniSection section, T item) where T : class
        {
            section.ReadFromINi(typeof(T), item);
        }

        public static void ReadFromINi(this ACBrIniSection section, Type tipo, object item)
        {
            if (!tipo.IsClass) return;

            foreach (var property in tipo.GetProperties(BindingFlags.Public | BindingFlags.Instance))
            {
                if (property.HasAttribute<IniIgnoreAttribute>()) continue;
                if (!(property.CanRead && property.CanWrite)) continue;
                if (!IniValueWrapper.CanWrapUnwrap(property.PropertyType)) continue;

                var keyName = property.HasAttribute<IniKeyAttribute>() ? property.GetAttribute<IniKeyAttribute>().Value : property.Name;

                if (!section.ContainsKey(keyName)) continue;

                var str = section[keyName];
                if (string.IsNullOrWhiteSpace(str)) continue;

                var value = IniValueWrapper.UnWrap(property.PropertyType, str, null);
                property.SetValue(item, value, null);
            }
        }

        #endregion Methods
    }
}