using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace ACBrLib.Core
{
    public sealed class ACBrIniSection : Dictionary<string, string>
    {
        #region Constructors

        public ACBrIniSection(string name) : this(null, name)
        {
        }

        public ACBrIniSection(ACBrIniFile parent, string name)
        {
            Parent = parent;
            Name = name;
        }

        #endregion Constructors

        #region Properties

        public ACBrIniFile Parent { get; internal set; }

        public string Name { get; set; }

        #endregion Properties

        #region Methods

        internal void Save(TextWriter stream)
        {
            stream.WriteLine($"[{Name}]");

            foreach (var iniData in this)
                stream.WriteLine($"{iniData.Key}={iniData.Value}");

            stream.WriteLine("");
        }

        public TType GetValue<TType>(string key, TType defaultValue = default)
        {
            if (string.IsNullOrEmpty(key) || string.IsNullOrWhiteSpace(key)) return defaultValue;

            TType ret;
            try
            {
                if (!ContainsKey(key)) return defaultValue;

                ret = IniValueWrapper.UnWrap(this[key], defaultValue);
            }
            catch (Exception)
            {
                ret = defaultValue;
            }

            return ret;
        }

        public void SetValue<TType>(string key, TType value)
        {
            Add(key, IniValueWrapper.Wrap<TType>(value));
        }

        public override string ToString()
        {
            var builder = new StringBuilder();

            using (var writer = new StringWriter(builder))
                Save(writer);

            return builder.ToString();
        }

        #endregion Methods
    }
}