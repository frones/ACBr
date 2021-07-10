using System;
using System.Collections.Generic;
using System.Globalization;
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

        public TType GetValue<TType>(string key, TType defaultValue = default, IFormatProvider format = null)
        {
            if (string.IsNullOrEmpty(key) || string.IsNullOrWhiteSpace(key)) return defaultValue;

            TType ret;
            try
            {
                if (format == null) format = CultureInfo.InvariantCulture;
                if (!ContainsKey(key)) return defaultValue;

                ret = (TType)Convert.ChangeType(this[key], typeof(TType), format);
            }
            catch (Exception)
            {
                ret = defaultValue;
            }

            return ret;
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