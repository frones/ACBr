using System;
using System.Collections.Generic;
using System.Globalization;

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

        public TType GetValue<TType>(string key, TType defaultValue = default(TType), IFormatProvider format = null)
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

        #endregion Methods
    }
}